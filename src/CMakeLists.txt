SET(Boost_ADDITIONAL_VERSIONS 1.46.1)

find_package(Boost COMPONENTS test_exec_monitor thread system filesystem)

include_directories(${Boost_INCLUDE_DIRS})

add_subdirectory(sqlite)

add_subdirectory(test)

add_subdirectory(elements)

add_subdirectory(osmdb)