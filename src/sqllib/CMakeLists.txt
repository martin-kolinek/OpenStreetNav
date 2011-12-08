add_subdirectory(strings)

file(GLOB SQLFILES *.sql)

set(TEMPL ${CMAKE_BINARY_DIR}/templ)
FILE(MAKE_DIRECTORY ${TEMPL})

set(PSQL_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../psql/psql.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/header_templ.h.in ${TEMPL}/header_templ.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test_templ.cc.in ${TEMPL}/test_templ.cc)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src_templ.cc.in ${TEMPL}/src_templ.cc)

add_custom_command(OUTPUT ${GENSRC}/sqllib_gen.cc ${INCDIR}/sqllib_gen_test.cc ${INCDIR}/sqllib_gen.h
                   COMMAND ${PERL_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/queries.pl" "${TEMPL}" "${GENSRC}" "${INCDIR}" ${SQLFILES}
                   DEPENDS ${TEMPL}/src_templ.cc ${TEMPL}/header_templ.h ${TEMPL}/test_templ.cc ${SQLFILES}
                   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                   COMMENT "Generating sqllib from sql files")

add_custom_target(sqllib_gtest
                DEPENDS ${INCDIR}/sqllib_gen_test.cc)

add_library(sqllib sqllib.cpp ${GENSRC}/sqllib_gen.cc)
target_link_libraries(sqllib psql sqllib_str)
