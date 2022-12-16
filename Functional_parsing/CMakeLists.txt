# CMakeList.txt : CMake project for Functional_parsing, include source and define
# project specific logic here.
#

# Add source to this project's executable.
add_executable (Functional_parsing "Functional_parsing.cpp" "Functional_parsing.h")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET Functional_parsing PROPERTY CXX_STANDARD 20)
endif()

# TODO: Add tests and install targets if needed.
