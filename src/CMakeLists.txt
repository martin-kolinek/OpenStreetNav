find_package(Boost REQUIRED COMPONENTS test_exec_monitor thread system filesystem signals program_options regex)
include_directories(${Boost_INCLUDE_DIRS})

find_package(GTKmm REQUIRED)
include_directories(${GTKmm_INCLUDE_DIRS})

find_package(PostgreSQL REQUIRED)
message(${POSTGRESQL_INCLUDE_DIR} ${LIBPQ_INCLUDE_DIR})
include_directories(${POSTGRESQL_INCLUDE_DIR} ${LIBPQ_INCLUDE_DIR})
link_directories(${POSTGRESQL_LIBRARY_DIR})

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

add_subdirectory(psql)

add_subdirectory(sqllib)

add_subdirectory(elements)

add_subdirectory(osmdb)

add_subdirectory(xmlparse)

add_subdirectory(importer)

add_subdirectory(projection)

add_subdirectory(geoelements)

add_subdirectory(displayer)

add_subdirectory(wayreduction)

add_subdirectory(pathfinding)

add_subdirectory(roads)

add_subdirectory(cost)

add_subdirectory(test)

add_subdirectory(road_db_creator)

add_subdirectory(edge_creator)