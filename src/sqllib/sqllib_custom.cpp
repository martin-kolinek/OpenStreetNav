#include "sqllib_custom.h"

namespace sqllib
{

psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> > get_toshow_edges_select(boost::property_tree::ptree const& entries, psql::Database& db, bool named, std::string name)
{
    KeyValFilterTranslator tr("ST_X(ST_StartPoint(e.Location::geometry)), ST_Y(ST_StartPoint(e.Location::geometry)), ST_X(ST_EndPoint(e.Location::geometry)), ST_Y(ST_EndPoint(e.Location::geometry)), e.WayID",
                              "Edges e INNER JOIN WayAttributes a ON a.WayID = e.WayID",
                              "e.Location && ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), -1)",
                              "a"
                             );
    auto cr = SqlCreatorFactory::create(tr.translate(entries));
    if (named)
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> >(name, cr->create_sql(), db);
    else
        return psql::Statement<psql::BindTypes<double, double, double, double>, psql::RetTypes<double, double, double, double, int64_t, double, double, double, double, int, int> >(cr->create_sql(), db);
}

}
