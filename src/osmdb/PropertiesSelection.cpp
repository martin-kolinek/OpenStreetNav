/*
 * PropertiesSelection.cpp
 *
 *  Created on: Jan 1, 2012
 *      Author: martin
 */

#include "PropertiesSelection.h"
#include "../sqllib/sqllib.h"
#include "PropertiesSelectionException.h"
#include "../util/tuple_sub.h"
#include <tuple>

namespace osmdb
{

PropertiesSelection::PropertiesSelection(OsmDatabase& osmdb):
    db(osmdb),
    get_node_attrs_st(sqllib::get_select_node_attributes(db.get_db(), true, "ps_node_attrs")),
    get_way_attrs_st(sqllib::get_select_way_attributes(db.get_db(), true, "ps_way_attrs")),
    get_rel_attrs_st(sqllib::get_select_rel_attributes(db.get_db(), true, "ps_rel_attrs")),
    get_waynodes_st(sqllib::get_select_waynodes(db.get_db(), true, "ps_waynodes")),
    get_node_members_st(sqllib::get_select_node_members(db.get_db(), true, "ps_node_members")),
    get_way_members_st(sqllib::get_select_way_members(db.get_db(), true, "ps_way_members")),
    get_rel_members_st(sqllib::get_select_rel_members(db.get_db(), true, "ps_rel_members")),
    get_node_pos_st(sqllib::get_select_position(db.get_db(), true, "ps_node_pos"))
{
}

std::set<std::pair<std::string, std::string> > PropertiesSelection::get_node_tags(int64_t id)
{
    return get_multimap<std::set<std::pair<std::string, std::string> > >(get_node_attrs_st, id);
}

std::set<std::pair<std::string, std::string> > PropertiesSelection::get_way_tags(int64_t id)
{
    return get_multimap<std::set<std::pair<std::string, std::string> > >(get_way_attrs_st, id);
}

std::set<std::pair<std::string, std::string> > PropertiesSelection::get_relation_tags(int64_t id)
{
    return get_multimap<std::set<std::pair<std::string, std::string> > >(get_rel_attrs_st, id);
}

std::map<int, int64_t> PropertiesSelection::get_waynodes(int64_t way_id)
{
    std::map<int, int64_t> ret;
    auto v = psql::exec_statement(get_waynodes_st, way_id);
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        ret.insert(std::make_pair(std::get<1>(*it), std::get<0>(*it)));
    }
    return ret;
}

std::multimap<std::string, int64_t> PropertiesSelection::get_node_members(int64_t rel_id)
{
    return get_multimap<std::multimap<std::string, int64_t> >(get_node_members_st, rel_id);
}

std::multimap<std::string, int64_t> PropertiesSelection::get_way_members(int64_t rel_id)
{
    return get_multimap<std::multimap<std::string, int64_t> >(get_way_members_st, rel_id);
}

std::multimap<std::string, int64_t> PropertiesSelection::get_relation_members(int64_t rel_id)
{
    return get_multimap<std::multimap<std::string, int64_t> >(get_rel_members_st, rel_id);
}

geo::Point PropertiesSelection::get_position(int64_t node_id)
{
    get_node_pos_st.execute(node_id);
    if (get_node_pos_st.row_count() != 1)
        throw PropertiesSelectionException(util::concatenate(" ", "Node with id ", node_id, "does not exist"));
    double a, b;
    std::tie(a, b) = get_node_pos_st.get_row(0);
    return geo::Point(b, a);
}

PropertiesSelection::~PropertiesSelection()
{
}

} /* namespace osmdb */
