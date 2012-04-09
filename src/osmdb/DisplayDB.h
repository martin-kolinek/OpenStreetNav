/*
 * DisplayDB.h
 *
 *  Created on: Nov 12, 2011
 *      Author: martin
 */

#ifndef DISPLAYDB_H_
#define DISPLAYDB_H_

#include "OsmDatabase.h"
#include "../displayer/DisplayProvider.h"
#include "ToShowSelectCollection.h"
#include "PropertiesSelection.h"
#include "../displayer/LineDisplayStyle.h"
#include "EdgeTranslator.h"
#include "../displayer/Descriptible.h"

namespace osmdb
{

/**
 * \class DisplayDB
 * Class that allows for drawing of data from database. Uses xml files to specify what to draw.
 */
class DisplayDB : public display::DisplayProvider
{
public:
    /**
     * Constructs a DisplayDB
     * @param db underlying OsmDatabase
     * @param path_base path to xml files specifying what to draw, this should be a directory containing files <min_zoom>.xml to <max_zoom>.xml
     * @param min_zoom minimum zoom level to draw
     * @param max_zoom maximum zoom level to draw
     */
    DisplayDB(osmdb::OsmDatabase& db, std::vector<std::string> schemas, int offset, std::shared_ptr<EdgeTranslator> const& transl);
    virtual ~DisplayDB();
    element_range get_display_elements();
    osmdb::OsmDatabase& get_db();
    void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    std::vector<std::shared_ptr<display::Descriptible> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom);
    double center_lat();
    double center_lon();
    int get_min_zoom();
    int get_max_zoom();
private:
    osmdb::OsmDatabase& db;
    std::vector<std::pair<osm::Edge, display::LineDisplayStyle> > display_elements;
    double clat;
    double clon;
    int minz, maxz;
    ToShowSelectCollection coll;
    std::shared_ptr<EdgeTranslator> transl;
    psql::Statement<psql::BindTypes<>, psql::RetTypes<double, double, double, double> > get_bounds;
};

} /* namespace osmdb */
#endif /* DISPLAYDB_H_ */
