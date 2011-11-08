find_package(Boost COMPONENTS test_exec_monitor thread system filesystem signals program_options)

link_directories(${Boost_LIBRARY_DIRS})

include_directories(${Boost_INCLUDE_DIRS})

find_package(PkgConfig)

pkg_check_modules(LIBXMLPP REQUIRED libxml++-2.6)

link_directories(${LIBXMLPP_LIBRARY_DIRS})

include_directories(${LIBXMLPP_INCLUDE_DIRS})

pkg_check_modules(SQLITE3 REQUIRED sqlite3)

link_directories(${SQLITE3_LIBRARY_DIRS} )

include_directories(${SQLITE3_INCLUDE_DIRS})

add_subdirectory(sqlite)

add_subdirectory(elements)

add_subdirectory(osmdb)

add_subdirectory(xmlparse)

add_subdirectory(importer)

add_subdirectory(test)