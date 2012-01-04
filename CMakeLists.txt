project(CMakeTest)
cmake_minimum_required(VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

IF(CMAKE_COMPILER_IS_GNUCC)
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c++0x -fmessage-length=0 -g -O0 -fno-inline -Wall -Wextra")
ENDIF(CMAKE_COMPILER_IS_GNUCC)
IF(CMAKE_COMPILER_IS_GNUCXX)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -fmessage-length=0 -g -O0 -fno-inline -Wall -Wextra")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)

find_package(Doxygen)

if (DOXYGEN_FOUND STREQUAL "NO")
message(FATAL_ERROR "Doxygen not found")
endif(DOXYGEN_FOUND STREQUAL "NO")

SET(DOXYGEN_OUTPUT_DIR "${CMAKE_BINARY_DIR}/api-doc")

SET(DOCUMENTATION_HTML yes CACHE BOOL "Whether to build doxygen html documentation")
SET(DOCUMENTATION_LATEX no CACHE BOOL "Whether to build doxygen latex documentation")

SET(DOCUMENTATION_HTML_HEADER "" CACHE STRING "The header doxygen should use with html")
SET(DOCUMENTATION_HTML_FOOTER "" CACHE STRING "The footer doxygen should use with html")

if (DOCUMENTATION_HTML)
    SET(DOXYGEN_HTML_DOC "YES")
else(DOCUMENTATION_HTML)
    SET(DOXYGEN_HTML_DOC "NO")
endif(DOCUMENTATION_HTML)
if (DOCUMENTATION_LATEX)
    SET(DOXYGEN_LATEX_DOC "YES")
else(DOCUMENTATION_LATEX)
    SET(DOXYGEN_LATEX_DOC "NO")
endif(DOCUMENTATION_LATEX)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/xmlschema/libxsd)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_BINARY_DIR}/Doxyfile)

add_custom_target(doxygen ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile)

enable_testing()

add_subdirectory(src)

