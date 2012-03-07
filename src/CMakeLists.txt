find_package(Boost REQUIRED COMPONENTS test_exec_monitor thread system filesystem signals program_options regex)
include_directories(${Boost_INCLUDE_DIRS})

find_package(Sqlite3 REQUIRED)
include_directories(${SQLITE3_INCLUDE_DIR})

find_package(GTKmm REQUIRED)
include_directories(${GTKmm_INCLUDE_DIRS})

find_package(PostgreSQL REQUIRED)
include_directories(${PostgreSQL_INCLUDE_DIRS})
link_directories(${PostgreSQL_LIBRARY_DIRS})

find_package(GEOS REQUIRED)
include_directories(${GEOS_INCLUDE_DIRS})

find_package(EXPAT REQUIRED)
include_directories(${EXPAT_INCLUDE_DIRS})

find_package(Perl REQUIRED)

SET(INCDIR ${CMAKE_BINARY_DIR}/include)
file(MAKE_DIRECTORY ${INCDIR})
include_directories(${INCDIR})

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/share)

SET(GENSRC ${CMAKE_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${GENSRC})
                    
add_subdirectory(util)

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

add_subdirectory(wayreduction)

add_subdirectory(pathfinding)

add_subdirectory(roads)

add_subdirectory(cost)

add_subdirectory(test)
