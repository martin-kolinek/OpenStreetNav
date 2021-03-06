#include "sqllib.h"

namespace sqllib
{

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> > get_toshow_edges_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("e.StartSequenceNo, e.StartNodeID, ST_X(ST_StartPoint(e.Location::geometry)), ST_Y(ST_StartPoint(e.Location::geometry)), e.EndSequenceNo, e.EndNodeID, ST_X(ST_EndPoint(e.Location::geometry)), ST_Y(ST_EndPoint(e.Location::geometry)), e.WayID",
                              "Edges e INNER JOIN WayAttributes a ON a.WayID = e.WayID",
                              "e.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), -1)",
                              "a",
                              std::vector<std::string> {"float8", "float8", "float8", "float8", "float8", "int", "int"},
                              "order by priority desc");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int, int64_t, double, double, int, int64_t, double, double, int64_t, double, double, double, double, double, int, int> >(cr->create_sql(), db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<> > get_edges_insert(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("wn1.WayID, wn1.SequenceNo, wn1.NodeID, wn2.SequenceNo, wn2.NodeID, ST_MakeLine(n1.Location::geometry, n2.Location::geometry)",
                              "WayNodes wn1 INNER JOIN Nodes n1 ON n1.ID = wn1.NodeID INNER JOIN WayNodes wn2 ON wn1.WayID = wn2.WayID AND wn1.NextSequenceNo = wn2.SequenceNo INNER JOIN Nodes n2 ON n2.ID = wn2.NodeID INNER JOIN WayAttributes a ON a.WayID = wn1.WayID",
                              "",
                              "a",
                              std::vector<std::string> {"float8", "float8", "float8", "float8", "float8", "int", "int"},
                              "order by priority desc");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    std::string insert = "INSERT INTO Edges (WayID, StartSequenceNo, StartNodeID, EndSequenceNo, EndNodeID, Location, Red, Green, Blue, Alpha, Thickness, Style, Priority) ";
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<> >(name, insert + cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<> >(insert + cr->create_sql(), db);
}

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> > get_selected_edges_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("e.WayID",
                              "Edges e INNER JOIN WayAttributes a ON a.WayID = e.WayID",
                              "ST_Intersects(e.Location, ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), -1))",
                              "a",
                              std::vector<std::string> {"float8", "float8", "float8", "float8", "float8", "int", "int"},
                              "order by priority desc");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<int64_t, double, double, double, double, double, int, int> >(cr->create_sql(), db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, int64_t, std::string, std::string> > get_wayreduction_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("wn.WayID as CurWayID, wn.SequenceNo as SeqNo, n.ID as NdID, ST_X(n.Location::geometry), ST_Y(n.Location::geometry), COALESCE(wn2.WayID, -1) AS CrossWayID, COALESCE(a2.Key, '') as CrossWayKey, COALESCE(a2.Value, '')",
                              "WayNodes wn INNER JOIN Nodes n ON wn.NodeID = n.ID INNER JOIN WayAttributes a ON a.WayID = wn.WayID LEFT JOIN WayNodes wn2 ON wn2.WayID != wn.WayID AND wn.NodeID = wn2.NodeID LEFT JOIN WayAttributes a2 ON a2.WayID = wn2.WayID",
                              "",
                              "a",
                              std::vector<std::string>(),
                              "order by CurWayID, SeqNo, NdID, CrossWayID, CrossWayKey");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, int64_t, std::string, std::string> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, int64_t, std::string, std::string> >(cr->create_sql(), db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, std::string, std::string> > get_way_attributes_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("a.WayID, COALESCE(a.Key, '') as Key, COALESCE(a.Value, '')",
                              "WayAttributes a LEFT JOIN WayAttributes a2 ON a.WayID = a2.WayID",
                              "",
                              "a2",
                              std::vector<std::string>(),
                              "order by WayID, Key");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, std::string, std::string> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, std::string, std::string> >(cr->create_sql(), db);
}

psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, std::string, std::string> > get_way_node_attributes_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("wn.WayID, wn.SequenceNo as SequenceNo, wn.NodeID, ST_X(n.Location::geometry), ST_Y(n.Location::geometry), COALESCE(na.Key, '') as Key, COALESCE(na.Value,'') as Value",
                              "WayNodes wn INNER JOIN Nodes n ON n.ID = wn.NodeID LEFT JOIN NodeAttributes na ON n.ID = na.NodeID INNER JOIN WayAttributes wa ON wa.WayID = wn.WayID",
                              "",
                              "wa",
                              std::vector<std::string>(),
                              "order by WayID, SequenceNo, NodeID, Key");
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, std::string, std::string> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t, int, int64_t, double, double, std::string, std::string> >(cr->create_sql(), db);
}

}
