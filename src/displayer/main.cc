/*
 * main.cc
 *
 *  Created on: Nov 14, 2011
 *      Author: martin
 */

#include <gtkmm/main.h>
#include <iostream>
#include <gtkmm/builder.h>
#include <gtkmm/window.h>
#include "MapDrawingArea.h"
#include "../osmdb/osmdb.h"
#include <memory>
#include <gtkmm/drawingarea.h>
#include <gtkmm/button.h>
#include <gtkmm/entry.h>
#include <gtkmm/spinbutton.h>
#include <gtkmm/textview.h>
#include <glibmm/optioncontext.h>
#include <glibmm/optiongroup.h>
#include <glibmm/optionentry.h>
#include <osmdisp_config.h>
#include "../util/util.h"
#include "EdgeHighlighter.h"
#include "LineDisplayStyle.h"
#include "../pathfinding/pathfinding.h"
#include <boost/regex.hpp>
#include <fstream>
#include <ctime>

class ZoomerDrawAreaConnector
{
public:
    ZoomerDrawAreaConnector(Gtk::SpinButton* b, display::MapDrawingArea* ar):
        b(b),
        ar(ar)
    {
    }
    Gtk::SpinButton* b;
    display::MapDrawingArea* ar;
    void update()
    {
        ar->set_zoom(b->get_adjustment()->get_value());
    }
};

void write_ptree(boost::property_tree::ptree const& ptree, std::ostream& ost, int depth)
{
    for (auto it = ptree.begin(); it != ptree.end(); ++it)
    {
        if (it->second.data() == "" && it->second.size() == 0)
            continue;
        for (int i = 0; i < depth; ++i)
            ost << "\t";
        ost << it->first << " ";
        ost << it->second.data();
        ost << std::endl;
        write_ptree(it->second, ost, depth + 1);
    }
}

std::string get_els_text(std::vector<std::unique_ptr<display::Descriptible> > const& els)
{
    std::ostringstream str;
    for (unsigned int i = 0; i < els.size(); ++i)
    {
        write_ptree(els[i]->get_description(), str, 0);
    }
    return str.str();
}

std::string get_els_text(display::Descriptible const& desc)
{
    std::ostringstream str;
    write_ptree(desc.get_description(), str, 0);
    return str.str();
}

Glib::OptionEntry make_entry(std::string lname, char sname, std::string desc = "")
{
    Glib::OptionEntry e;
    e.set_long_name(lname);
    e.set_short_name(sname);
    e.set_description(desc);
    return e;
}

osm::WayRegion get_way_region_from_el(osm::Element const& el)
{
    osm::Way const& w = static_cast<osm::Way const&>(el);
    return osm::WayRegion(w);
}

class SearchButtonConnector
{
private:
    display::MapDrawingArea* area;
    display::EdgeHighlighter& high;
    pathfind::PathFinder& finder;
    Gtk::Entry* startentry;
    Gtk::Entry* endentry;
    Gtk::TextView* view;
public:
    SearchButtonConnector(display::MapDrawingArea* area, display::EdgeHighlighter& high, pathfind::PathFinder& finder, Gtk::Entry* startentry, Gtk::Entry* endentry, Gtk::TextView* view):
        area(area),
        high(high),
        finder(finder),
        startentry(startentry),
        endentry(endentry),
        view(view)
    {
    }
    void search_click()
    {
        high.clear();
        int64_t start, end;
        start = util::parse<int64_t>(startentry->get_text());
        end = util::parse<int64_t>(endentry->get_text());
        std::clock_t stime = std::clock();
        auto r = finder.find_way(start, end);
        stime = std::clock() - stime;
        std::ostringstream str;
        str << "Route searching took " << ((double)stime / CLOCKS_PER_SEC) << " seconds" << std::endl;
        view->get_buffer()->set_text(str.str() + get_els_text(r));
        high.add_descriptible(r);
        area->refresh();
    }
};

std::pair<int, std::vector<std::string> > extract_zooms(std::string const& file)
{
    std::pair<int, std::vector<std::string> > ret;
    std::ifstream fs(file, std::ifstream::in);
    std::string line;
    boost::regex r("^\\s*(\\d+)\\s+(\\S+)\\s*$");
    bool first = true;
    while (!fs.eof())
    {
        std::getline(fs, line);
        boost::smatch match;
        if (!boost::regex_match(line, match, r))
            continue;
        unsigned int i = util::parse<int>(match[1]);
        if (first)
        {
            ret.first = i;
            first = false;
        }
        std::cout << ret.second.size() << " " << ret.first << " " << i << std::endl;
        while (ret.second.size() + ret.first < i)
            ret.second.push_back(ret.second.back());
        ret.second.push_back(match[2]);
    }
    return ret;
}

int main(int argc, char** argv)
{
    Glib::ustring dbname;
    Glib::ustring schema;
    Glib::ustring road_sch;
    Glib::ustring config;
    double lat = -500;
    double lon = -500;
    int zoom = -1;
    Glib::OptionGroup gr("displayer", "displayer options");
    auto e1 = make_entry("database", 'd', "database to connect to");
    auto e2 = make_entry("schema", 's', "additional schema to look into");
    auto e3 = make_entry("latitude", 'y', "starting center latitude");
    auto e4 = make_entry("longitude", 'x', "starting center longitude");
    auto e5 = make_entry("zoom", 'z', "starting zoom level");
    auto e6 = make_entry("road-schema", 'r', "schema containing road edges");
    auto e7 = make_entry("config", 'c', "configuration file containing schemas for zooms");
    gr.add_entry(e1, dbname);
    gr.add_entry(e2, schema);
    gr.add_entry(e3, lat);
    gr.add_entry(e4, lon);
    gr.add_entry(e5, zoom);
    gr.add_entry(e6, road_sch);
    gr.add_entry(e7, config);
    Glib::OptionContext cxt;
    cxt.add_group(gr);
    try
    {
        Gtk::Main kit(argc, argv, cxt);
        if (config == "")
        {
            std::cout << "Config file required" << std::endl;
            return 1;
        }
        Glib::RefPtr<Gtk::Builder> bldr = Gtk::Builder::create_from_file(GLADE_PATH);
        Gtk::Window* wnd = 0;
        bldr->get_widget("window1", wnd);
        display::MapDrawingArea* area = 0;
        bldr->get_widget_derived("drawingarea1", area);
        Gtk::Button* searchbutton;
        bldr->get_widget("searchbutton", searchbutton);
        Gtk::Entry* startentry;
        Gtk::Entry* endentry;
        bldr->get_widget("startidentry", startentry);
        bldr->get_widget("endidentry", endentry);
        Gtk::SpinButton* zoomer;
        bldr->get_widget("zoomspinbutton", zoomer);
        ZoomerDrawAreaConnector conn(zoomer, area);
        zoomer->get_adjustment()->signal_value_changed().connect(sigc::mem_fun(&conn, &ZoomerDrawAreaConnector::update));

        Gtk::TextView* view;
        bldr->get_widget("textview1", view);

        std::string conninfo;
        if (dbname == "")
            conninfo = "";
        else
            conninfo = "dbname=" + dbname;
        psql::Database pdb(conninfo);
        if (schema != "")
            pdb.set_schema(schema);
        osmdb::OsmDatabase odb(pdb);
        zoomer->get_adjustment()->set_value(area->get_zoom());
        auto p = extract_zooms(config);
        if (p.second.size() == 0)
        {
            std::cout << "Error extracting schemas for individual zoom levels" << std::endl;
            return 1;
        }
        std::shared_ptr<osmdb::DisplayDB> dispdb(new osmdb::DisplayDB(odb, p.second, p.first));
        area->add_dp(1, dispdb);
        std::shared_ptr<display::EdgeHighlighter> high(new display::EdgeHighlighter(*dispdb, display::LineDisplayStyle(0, 1, 1, 1, 2.5, false)));
        area->add_dp(3, high);
        std::shared_ptr<display::EdgeHighlighter> high_path(new display::EdgeHighlighter(*dispdb, display::LineDisplayStyle(1, 1, 1, 1, 2.5, false)));
        area->add_dp(2, high_path);
        area->zoom_changed.connect([zoomer](int val)
        {
            zoomer->get_adjustment()->set_value(val);
        });
        area->element_clicked.connect([view, &high, area](std::vector<std::unique_ptr<display::Descriptible> > const & els)
        {
            view->get_buffer()->set_text(get_els_text(els));
            high->clear();
            for (auto it = els.begin(); it != els.end(); ++it)
            {
                high->add_descriptible(**it);
            }
            area->refresh();
        });
        if (zoom >= 1 && zoom <= 15)
            area->set_zoom(zoom);
        if (lat >= -90 && lat <= 90)
            area->set_latitude(lat);
        if (lon >= -180 && lon <= 180)
            area->set_longitude(lon);

        std::unique_ptr<pathfind::PathFinder> finder;

        if (road_sch != "")
        {
            psql::Database rpdb(conninfo);
            rpdb.set_schema(road_sch);
            osmdb::OsmDatabase rdb(rpdb);
            osmdb::RoadLister rl(rdb);
            finder = std::unique_ptr<pathfind::PathFinder>(new pathfind::PathFinder(rl));
            SearchButtonConnector searchconn(area, *high_path, *finder, startentry, endentry, view);
            searchbutton->signal_clicked().connect(sigc::mem_fun(&searchconn, &SearchButtonConnector::search_click));
        }
        area->show();

        kit.run(*wnd);
    }
    catch (Glib::Error& err)
    {
        std::cout << err.what() << std::endl;
    }
    catch (std::exception& err)
    {
        std::cout << "Error running displayer: " << std::endl << err.what() << std::endl;
        return 1;
    }
}


