/*
 * PropertiesSelection.h
 *
 *  Created on: Jan 1, 2012
 *      Author: martin
 */

#ifndef PROPERTIESSELECTION_H_
#define PROPERTIESSELECTION_H_

#include "../psql/psql.h"
#include "OsmDatabase.h"

namespace osmdb
{

class PropertiesSelection
{
public:
    PropertiesSelection(OsmDatabase& db);
    std::set<std::pair<std::string, std::string> > get_node_tags(int64_t id);
    std::set<std::pair<std::string, std::string> > get_way_tags(int64_t id);
    std::set<std::pair<std::string, std::string> > get_relation_tags(int64_t id);
    std::vector<int64_t> get_waynodes(int64_t way_id);
    std::multimap<std::string, int64_t> get_node_members(int64_t rel_id);
    std::multimap<std::string, int64_t> get_way_members(int64_t rel_id);
    std::multimap<std::string, int64_t> get_relation_members(int64_t rel_id);
    geo::Point get_position(int64_t node_id);
    virtual ~PropertiesSelection();
private:
    OsmDatabase& db;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string> > get_node_attrs_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string> > get_way_attrs_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, std::string> > get_rel_attrs_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<int64_t> > get_waynodes_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t> > get_node_members_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t> > get_way_members_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<std::string, int64_t> > get_rel_members_st;
    psql::Statement<psql::BindTypes<int64_t>, psql::RetTypes<double, double> > get_node_pos_st;
    template <typename Cont, typename K, typename V, typename... Args>
    Cont get_multimap(psql::Statement<psql::BindTypes<Args...>, psql::RetTypes<K, V> >& st, Args... a)
    {
        st.execute(a...);
        Cont ret;
        for (int i = 0; i < st.row_count(); ++i)
        {
            auto const& r = st.get_row(i);
            ret.insert(std::make_pair(std::get<0>(r), std::get<1>(r)));
        }
        return ret;
    }

};

} /* namespace osmdb */
#endif /* PROPERTIESSELECTION_H_ */
