#include <iostream>
#include <string_view>
#include <string>
#include <optional>
#include <cassert>
#include <functional>

template <typename T>
using Parsed_t = std::optional<std::pair<T, std::string_view>>;

//auto parser(std::string_view input)->Parsed_t<T>;

//static_assert(std::same_as<int, int>);
//static_assert(std::regular_invocable<decltype(::atoi), char const*>);
//static_assert(std::regular_invocable<decltype(::rand)>);

template <typename P>
concept Parser =
	std::regular_invocable<P, std::string_view> &&
	requires (std::invoke_result_t<P, std::string_view> result) {
		std::same_as<
			decltype(result),
			Parsed_t<typename decltype(result)::value_type::first_type>>;
	};

template <Parser P>
using Parser_result_t = std::invoke_result_t<P, std::string_view>;

template <Parser P>
using Parser_value_t = typename Parser_result_t<P>::value_type::first_type;

inline constexpr auto
item = [](std::string_view input) -> Parsed_t<char>
{
	if (input.empty()) {
		return {};
	} else {
		return { {input[0], input.substr(1)} };
	}
};

static_assert(Parser<decltype(item)>);
static_assert(item("foo") == Parsed_t<char>{ {'f', "oo"}});
static_assert(item("") == Parsed_t<char>{});

template <typename T>
inline constexpr auto
empty = [](std::string_view) -> Parsed_t<T>
{
	return {};
};
static_assert(Parser<decltype(empty<int>)>);
static_assert(empty<char>("foo") == Parsed_t<char>{});
static_assert(empty<int>("") == Parsed_t<int>{});

template <typename F, typename... Args>
concept Parser_combinator =
	std::regular_invocable<F, Args...> &&
	Parser<std::invoke_result_t<F, Args...>>;

template <typename F, typename... Args>
requires Parser_combinator<F, Args...>
using Parser_combinator_value_t = std::invoke_result_t<F, Args...>;

constexpr Parser auto
str(std::string_view match)
{
	return [match](std::string_view input) -> Parsed_t<std::string>
	{
		if (input.starts_with(match)) {
			return { {
				std::string{match}, {input.begin() + match.size(), input.end()}
			} };
		}
		else {
			return {};
		}
	};
}
static_assert(Parser_combinator<decltype(str), std::string_view>);

template <typename T>
constexpr Parser auto
unit(T const& thing)
{
	return [thing](std::string_view input) -> Parsed_t<T>
	{
		return { {thing, input} };
	};
}

static_assert(Parser_combinator<decltype(unit<char>), char>);
static_assert(unit('x')("foo") == Parsed_t<char>{ {'x', "foo"}});
static_assert(unit(42)("") == Parsed_t<int>{ {42, ""}});

// Try one Parser and invoke a second Parser if the first one fails
template <Parser P, Parser Q>
requires std::convertible_to<Parser_value_t<P>, Parser_value_t<Q>>
constexpr Parser auto
operator|(P p, Q q)
{
	return [=](std::string_view input) -> Parser_result_t<Q>
	{
		if (auto const& result = std::invoke(p, input); result) {
			return result;
		} else {
			return std::invoke(q, input);
		}
	};
}

static_assert((empty<char> | item | unit('x'))("") == unit('x')(""));

constexpr Parser auto
choice(Parser auto parser, Parser auto... parsers)
{
	// it must failed if first argument is just free function because it's function pointer which just pointers
	// and bitwise operator for pointers are different and compiler can't choose
	// but it works on MSVC 2022 Version 17.5.0 Preview 1.0
	if constexpr (std::is_pointer_v<decltype(parser)>) {
		return (
			[parser](auto input) { return std::invoke(parser, input); }
			| ... |
			parsers
		);
	} else {
		return (parser | ... | parsers); // Binary left fold
	}
}

static_assert((empty<char> | item | unit('x'))("") == unit('x')(""));
static_assert(choice(empty<char>, item, unit('x'))("") == unit('x')(""));
static_assert(choice(item, empty<char>, unit('x'))("") == unit('x')(""));

// Compose a Parser with a unary function returning a Parser
template <Parser P, Parser_combinator<Parser_value_t<P>> F>
constexpr Parser auto
operator&(P parser, F func)
{
	using Parser_t = Parser_combinator_value_t<F, Parser_value_t<P>>;
	return [=](std::string_view input) -> Parser_result_t<Parser_t>
	{
		if (auto const& result = std::invoke(parser, input); result) {
			return std::invoke(std::invoke(func, result->first), result->second);
		} else {
			return {};
		}
	};
}

static_assert((item & unit<char>)("foo") == item("foo"));

constexpr Parser auto
chain(Parser auto parser, auto... funcs)
{
	if constexpr(std::is_pointer_v<decltype(parser)>) {
		return (
			[parser](auto input){ return std::invoke(parser, input); }
			& ... &
			funcs
		);
	} else {
		return (parser & ... & funcs);
	}
}

static_assert((item & unit<char>)("foo") == item("foo"));
static_assert(chain(item, unit<char>)("foo") == item("foo"));

// Invoke one Parser, throw away its result and invoke a second Parser
//constexpr Parser auto
//skip(Parser auto p, Parser auto q)
//{
//	return [=](std::string_view input)
//	{
//		if (auto const& result = std::invoke(p, input); result) {
//			return std::invoke(q, result->second);
//		} else {
//			return std::invoke(q, input);
//		}
//	};
//}

// Alternative version, using a composition of choice and chain
constexpr Parser auto
skip(Parser auto p, Parser auto q)
{
	return choice(chain(p, [q](auto const&){ return q; }), q);
}

static_assert(skip(item, unit('x'))("foo") == Parsed_t<char>{{'x', "oo"}});

// A Parser that consumes one digit
//auto digit(std::string_view input) ->Parsed_t<char>
//{
//	if (!input.empty() && ::isdigit(input[0])) {
//		return {{input[0], input.substr(1)}};
//	} else {
//		return {};
//	}
//}

// Return a Parser that applies a std::predicate to another Parser
template <typename Pr, Parser P = decltype(item)>
requires std::predicate<Pr, Parser_value_t<P>>
constexpr Parser auto
satisfy(Pr pred, P parser = item)
{
	return chain(
		parser,
		[pred](auto const& th) -> Parser auto
		{
			return [pred, th](std::string_view input) -> Parsed_t<Parser_value_t<P>>
			{
				if (std::invoke(pred, th))
					return {{ th, input }};
				else
					return {};
			};
		}
	);
}

Parser auto digit = satisfy(::isdigit);

Parser auto lower = satisfy(::islower);

Parser auto upper = satisfy(::isupper);

Parser auto letter = choice(lower, upper);

Parser auto alphanum = choice(letter, digit);

constexpr Parser auto
symbol(char x)
{
	return satisfy([x](char y){ return x == y; });
}

constexpr Parser auto plus = symbol('+');

template <typename T, Parser P, std::regular_invocable<T, Parser_value_t<P>> F>
requires std::convertible_to<std::invoke_result_t<F, T, Parser_value_t<P>>, T>
class reduce_many
{
	T init;
	P parser;
	F func;
public:

	reduce_many(T const& thing, P const& p, F const& fn)
		: init{thing}, parser{p}, func{fn}
	{}

	constexpr auto operator()(std::string_view input) const -> Parsed_t<T>
	{
		return choice(
			chain(parser, [this](auto const& thing)
			{
				return reduce_many{std::invoke(func, init, thing), parser, func};
			}),
			unit(init)
		)(input);
	}
};

// Repeat a char parser 0+ times and concatenate the result into a string
template <Parser P>
requires std::same_as<Parser_value_t<P>, char>
constexpr Parser auto
many(P parser)
{
	return reduce_many(
		std::string{},
		parser,
		[](std::string const& str, char ch){ return str + ch; }
	);
}

Parser auto whitespace = many(satisfy(::isspace));

constexpr Parser auto
token(Parser auto parser)
{
	return chain(
		skip(whitespace, parser),
		[](auto const& thing){ return skip(whitespace, unit(thing)); }
	);
}

// Repeat a char parser 1+ times and concatenate the result into a string
template <Parser P>
requires std::same_as<Parser_value_t<P>, char>
constexpr Parser auto
some(P parser)
{
	return chain(
		parser,
		[=](char ch)
		{
			return chain(
				many(parser),
				[=](std::string const& str){ return unit(std::string(1, ch) + str); }
			);
		}
	);
}

Parser auto natural = chain(
	some(digit),
	[](std::string const& digits){ return unit(std::stoi(digits)); }
);

Parser auto integer = choice(
	natural,
	chain(
		symbol('-'),
		[](auto) { return natural; },
		[](int nat) { return unit(-nat); }
	)
);

template <typename F>
constexpr decltype(auto)
papply(F&& f)
{
	return [f]<typename... Args>(Args&&... args)
	{
		papply(std::forward<F>(f), std::forward<Args>(args)...);
	};
};

template <typename F, typename... Args>
 constexpr decltype(auto)
papply(F&& f, Args&&... args)
{
	if constexpr(std::invocable<F, Args...>) {
		return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
	} else { // Assuming too few args to invoke f
		return std::bind_front(std::forward<F>(f), std::forward<Args>(args)...);
	}
};

constexpr int sum_3(int x, int y, int z) { return x + y + z; }

static_assert(papply(sum_3, 4, 5, 6) == 15);
static_assert(papply(sum_3, 4, 5)(6) == 15);
static_assert(papply(sum_3, 4)(5, 6) == 15);
//static_assert(papply(sum_3)(4)(5)(6) == 15); // don't compile :((( try fix later

// Combine two Parsers using the Callable returned by the first Parser
template <Parser P, Parser Q>
constexpr Parser auto
operator^(P p, Q q)
{
	using Result_t = std::invoke_result_t<decltype(papply), Parser_value_t<P>, Parser_value_t<Q>>;
	return [=](std::string_view input) -> Parsed_t<Result_t>
	{
		if (auto const& pr = std::invoke(p, input); pr) {
			if (auto const& qr = std::invoke(q, pr->second); qr) {
				return {{ papply(pr->first, qr->first), qr->second }};
			} else {
				return {};
			}
		} else {
			return {};
		}
	};
}

template <typename F, Parser... Ps>
requires std::regular_invocable<F, Parser_value_t<Ps>...>
constexpr Parser auto
sequence(F func, Ps... parsers)
{
	return (unit(func) ^ ... ^ parsers);
}


int main()
{
	assert(str("foo")("foobar")->first == "foo");
	
	/*auto ret = chain(item, unit<char>)("foo");
	std::cout << ret.value().first << ret.value().second;*/

	assert(many(symbol('+'))("+++---")->first == "+++");
	assert(many(symbol('+'))("---+++")->first == "");

	assert(token(str("foo"))("   foo  bar   ")->first == "foo");
	assert(token(str("foo"))("   foo  bar   ")->second == "bar   ");

	assert(some(symbol('+'))("+++---")->first == "+++");
	assert(some(symbol('+'))("---+++").has_value() == false);

	assert(integer("42")->first == 42);
	assert(integer("-42")->first == -42);

	Parser auto sum3_parser = sequence(
		[](int x, int y, int z) { return x + y + z; },
		token(integer),
		token(integer),
		token(integer)
	);

	assert(sum3_parser("4 5 6")->first == 15);
}