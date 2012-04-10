add_subdirectory(strings)

set(SQLFILES 
    create_edges_endnode_fkey.sql
    create_edges_location_index.sql
    create_edges_pkey.sql
    create_edges_startnode_fkey.sql
    create_edges_table.sql
    create_edges_wayid_fkey.sql
    create_nodeattributes_nodes_fkey.sql
    create_nodeattributes_pkey.sql
    create_node_attributes.sql
    create_nodeattr_keyval_index.sql
    create_nodemembers_node_fkey.sql
    create_nodemembers_pkey.sql
    create_nodemembers_relation_fkey.sql
    create_node_members.sql
    create_nodes_loc_index.sql
    create_nodes_pkey.sql
    create_nodes_table.sql
    create_relationattributes_pkey.sql
    create_relationattributes_relation_fkey.sql
    create_relation_attributes.sql
    create_relation_members.sql
    create_relations_pkey.sql
    create_relations_table.sql
    create_relattr_keyval_index.sql
    create_test_table.sql
    create_toshow_table.sql
    create_wayattributes_pkey.sql
    create_way_attributes.sql
    create_wayattributes_ways_fkey.sql
    create_wayattr_keyval_index.sql
    create_waymembers_pkey.sql
    create_waymembers_relation_fkey.sql
    create_way_members_table.sql
    create_waymembers_way_fkey.sql
    create_waynodes_node_fkey.sql
    create_waynodes_pkey.sql
    create_waynodes_table.sql
    create_waynodes_way_fkey.sql
    create_ways_pkey.sql
    create_ways_table.sql
    drop_edges_endnode_fkey.sql
    drop_edges_location_index.sql
    drop_edges_pkey.sql
    drop_edges_startnode_fkey.sql
    drop_edges_wayid_fkey.sql
    drop_nodeattributes_nodes_fkey.sql
    drop_nodeattributes_pkey.sql
    drop_nodeattr_keyval_index.sql
    drop_nodemembers_node_fkey.sql
    drop_nodemembers_pkey.sql
    drop_nodemembers_relation_fkey.sql
    drop_nodes_loc_index.sql
    drop_nodes_pkey.sql
    drop_relationattributes_pkey.sql
    drop_relationattributes_relation_fkey.sql
    drop_relations_pkey.sql
    drop_relattr_keyval_index.sql
    drop_wayattributes_pkey.sql
    drop_wayattributes_ways_fkey.sql
    drop_wayattr_keyval_index.sql
    drop_waymembers_pkey.sql
    drop_waymembers_relation_fkey.sql
    drop_waymembers_way_fkey.sql
    drop_waynodes_node_fkey.sql
    drop_waynodes_pkey.sql
    drop_waynodes_way_fkey.sql
    drop_ways_pkey.sql
    get_properties.sql
    insert_node_attr.sql
    insert_node.sql
    insert_relation.sql
    insert_test_table.sql
    insert_toshow.sql
    insert_way_attr.sql
    insert_way_node.sql
    insert_way.sql
    select_bounds.sql
    select_edges_in_box.sql
    select_node_descr_in_box.sql
    select_nodes_in_box.sql
    select_wayattributes_between_wayids.sql
    select_ways.sql
    test_select.sql
    import.sql
    road_network.sql
    insert_edge.sql
    insert_edges_from_roads.sql
    )

set(TEMPL ${CMAKE_BINARY_DIR}/templ)
FILE(MAKE_DIRECTORY ${TEMPL})

set(PSQL_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../psql/psql.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/header_templ.h.in ${TEMPL}/header_templ.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test_templ.cc.in ${TEMPL}/test_templ.cc)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src_templ.cc.in ${TEMPL}/src_templ.cc)

add_custom_command(OUTPUT ${GENSRC}/sqllib_gen.cc ${INCDIR}/sqllib_gen_test.cc ${INCDIR}/sqllib_gen.h
                   COMMAND ${PERL_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/queries.pl" "${TEMPL}" "${GENSRC}" "${INCDIR}" ${SQLFILES}
                   DEPENDS ${TEMPL}/src_templ.cc ${TEMPL}/header_templ.h ${TEMPL}/test_templ.cc ${SQLFILES} queries.pl
                   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                   COMMENT "Generating sqllib from sql files")

add_custom_target(sqllib_gtest
                DEPENDS ${INCDIR}/sqllib_gen_test.cc)

add_library(sqllib sqllib.cpp ${GENSRC}/sqllib_gen.cc)
target_link_libraries(sqllib psql sqllib_str)
