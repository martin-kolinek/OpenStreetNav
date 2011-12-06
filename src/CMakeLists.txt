find_package(Boost COMPONENTS test_exec_monitor thread system filesystem signals program_options regex)

if(Boost_FOUND)
  link_directories(${Boost_LIBRARY_DIRS})
  include_directories(${Boost_INCLUDE_DIRS})
else()
  message(FATAL_ERROR "Boost not found")
endif()

find_package(PkgConfig)

pkg_check_modules(LIBXMLPP REQUIRED libxml++-2.6)

link_directories(${LIBXMLPP_LIBRARY_DIRS})

include_directories(${LIBXMLPP_INCLUDE_DIRS})

pkg_check_modules(SQLITE3 REQUIRED sqlite3)

link_directories(${SQLITE3_LIBRARY_DIRS} )

include_directories(${SQLITE3_INCLUDE_DIRS})

pkg_check_modules(GTKMM REQUIRED gtkmm-3.0)

link_directories(${GTKMM_LIBRARY_DIRS})

include_directories(${GTKMM_INCLUDE_DIRS})

find_package(PostgreSQL)
if(PostgreSQL_FOUND)
  include_directories(${PostgreSQL_INCLUDE_DIRS})
  link_directories(${PostgreSQL_LIBRARY_DIRS})
else()
  message(FATAL_ERROR "PostgreSQL not found")
endif()

find_package(GEOS)
if(GEOS_FOUND)
  include_directories(${GEOS_INCLUDE_DIRS})
else()
  message(FATAL_ERROR "GEOS not found")
endif()

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
include_directories(${CMAKE_BINARY_DIR}/include)

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/share)

add_library(util util.cpp)

add_subdirectory(sqlite)

add_subdirectory(psql)

add_subdirectory(sqllib)

add_subdirectory(elements)

add_subdirectory(wkb)

add_subdirectory(osmdb)

add_subdirectory(xmlparse)

add_subdirectory(importer)

add_subdirectory(projection)

add_subdirectory(geoelements)

add_subdirectory(sqlitetest)

add_subdirectory(displayer)

add_subdirectory(test)