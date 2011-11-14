/*
 * DisplayDB.cpp
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#include "DisplayDB.h"

namespace display
{

DisplayDB::DisplayDB(const std::string& filename)
    : db(filename)
{
    sqlite::execute_sql("ATTACH DATABASE ':memory:' AS small;", db.get_db());
    sqlite::execute_sql(db.get_nodes_create("small." + db.nodes_table), db.get_db());
    sqlite::execute_sql(db.get_edges_create("small." + db.edges_table), db.get_db());
    sqlite::execute_sql(db.get_attributes_create("small." + db.attributes_table), db.get_db());
    copy_stmt1 = sqlite::Statement("INSERT INTO small." + db.nodes_table + " (ID, Latitude, Longitude) " +
                                   "SELECT n.ID, n.Latitude, n.Longitude FROM " + db.nodes_table + " n " +
                                   "INNER JOIN " + db.attributes_table + " a ON a.ObjectType=?1 AND a.ObjectID=n.ID " +
                                   "INNER JOIN " + db.to_show_table + " t ON t.Key=a.Key and t.Value=a.Value " +
                                   "WHERE n.Latitude >= ?4 AND n.Latitude <= ?5 AND n.Longitude >=?6 AND n.Longitude<=?7 AND t.MinZoom<=?3 AND t.MaxZoom>=?3" +
                                   "UNION SELECT n.ID, n.Latitude, n.Longitude FROM " + db.nodes_table + " n " +
                                   "INNER JOIN " + db.edges_table + " e ON e.StartNodeID = n.ID " +
                                   "INNER JOIN " + db.attributes_table + " a ON a.ObjectType=?2 AND a.ObjectID=e.WayID " +
                                   "INNER JOIN " + db.to_show_table + " t ON t.Key=a.Key AND t.Value=a.Value " +
                                   "WHERE t.MinZoom>=?3 AND t.MaxZoom<=?3 " +
                                   "UNION SELECT n.ID, n.Latitude, n.Longitude FROM Nodes n " +
                                   "INNER JOIN " + db.edges_table + " e ON e.EndNodeID = n.ID " +
                                   "INNER JOIN " + db.attributes_table + " a ON a.ObjectType=?2 AND a.ObjectID=e.WayID " +
                                   "INNER JOIN " + db.to_show_table + " t ON t.Key=a.Key AND t.Value=a.Value " +
                                   "WHERE t.MinZoom>=?3 AND t.MaxZoom<=?3", db.get_db());
    copy_stmt2 = sqlite::Statement("INSERT INTO small." + db.edges_table + " (WayID, StartNodeID, EndNodeID) " +
                                   "SELECT e.WayID, e.StartNodeID, e.EndNodeID FROM " + db.edges_table + " e " +
                                   "INNER JOIN small." + db.nodes_table + " n ON n.ID = e.StartNodeID " +
                                   "UNION SELECT e.WayID, e.StartNodeID, e.EndNodeID FROM " + db.edges_table + " e " +
                                   "INNER JOIN small." + db.nodes_table + " n ON n.ID = e.EndNodeID", db.get_db());
    clear_stmt1 = sqlite::Statement("DELETE FROM small." + db.nodes_table, db.get_db());
    clear_stmt2 = sqlite::Statement("DELETE FROM small." + db.edges_table, db.get_db());
    select_stmt1 = sqlite::Statement("SELECT ID, Latitude, Longitude FROM small." + db.nodes_table, db.get_db());
    select_stmt2 = sqlite::Statement("SELECT WayID, StartNodeID, EndNodeID FROM small." + db.edges_table, db.get_db());
}

DisplayDB::~DisplayDB()
{
}

const std::vector<osm::Edge> & DisplayDB::get_edges()
{
    return edges;
}

std::unordered_map<int64_t, osm::Node> DisplayDB::get_nodes()
{
    return nodes;
}

const std::vector<int64_t> & DisplayDB::get_free_nodes()
{
    return free_nodes;
}

void DisplayDB::set_bounds(const geo::Point& topleft, const geo::Point& bottomright, int zoom)
{
    clear_stmt1.step();
    clear_stmt2.step();
    copy_stmt1.bind((int)(osm::ObjectType::Node), (int)(osm::ObjectType::Way), zoom, bottomright.lat, topleft.lat, topleft.lon, bottomright.lon);
    copy_stmt1.step();
    copy_stmt2.step();
    nodes.clear();
    edges.clear();
    while (!select_stmt1.done())
    {
        if (select_stmt1.has_row())
        {
            nodes[select_stmt1.val_int64(0)] = osm::Node(select_stmt1.val_int64(0), select_stmt1.val_double(1), select_stmt1.val_double(2));
        }
        select_stmt1.step();
    }
    select_stmt1.reset();
    while (!select_stmt2.done())
    {
        if (select_stmt2.has_row())
        {
            edges.push_back(osm::Edge(select_stmt2.val_int64(1), select_stmt2.val_int64(2), select_stmt2.val_int64(0)));
        }
        select_stmt2.step();
    }
    select_stmt2.reset();
}

} /* namespace display */
