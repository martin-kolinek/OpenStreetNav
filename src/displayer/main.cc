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
#include <gtkmm/spinbutton.h>
#include <gtkmm/textview.h>
#include <glibmm/optioncontext.h>
#include <glibmm/optiongroup.h>
#include <glibmm/optionentry.h>
#include "config.h"

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

std::string get_els_text(std::vector<std::unique_ptr<osm::Element> > const& els)
{
    std::ostringstream str;
    str << "clicked" << std::endl;
    for (unsigned int i = 0; i < els.size(); ++i)
    {
        str << els[i]->get_type_str() << " " << els[i]->get_id() << std::endl;
        for (unsigned int j = 0; j < els[i]->get_tags().size(); ++j)
        {
            str << "\t" << els[i]->get_tags()[j].key << " " << els[i]->get_tags()[j].value << std::endl;
        }
    }
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

int main(int argc, char** argv)
{
    Glib::ustring dbname;
    Glib::ustring schema;
    double lat = -500;
    double lon = -500;
    int zoom = -1;
    Glib::OptionGroup gr("displayer", "displayer options");
    auto e1 = make_entry("database", 'd', "database to connect to");
    auto e2 = make_entry("schema", 's', "additional schema to look into");
    auto e3 = make_entry("latitude", 'y', "starting center latitude");
    auto e4 = make_entry("longitude", 'x', "starting center longitude");
    auto e5 = make_entry("zoom", 'z', "starting zoom level");
    gr.add_entry(e1, dbname);
    gr.add_entry(e2, schema);
    gr.add_entry(e3, lat);
    gr.add_entry(e4, lon);
    gr.add_entry(e5, zoom);
    Glib::OptionContext cxt;
    cxt.add_group(gr);
    try
    {
        Gtk::Main kit(argc, argv, cxt);

        Glib::RefPtr<Gtk::Builder> bldr = Gtk::Builder::create_from_file(GLADE_PATH);
        Gtk::Window* wnd = 0;
        bldr->get_widget("window1", wnd);
        display::MapDrawingArea* area = 0;
        bldr->get_widget_derived("drawingarea1", area);

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
            psql::execute_sql(pdb, "SET search_path TO " + schema + ", public");
        osmdb::OsmDatabase odb(pdb);
        zoomer->get_adjustment()->set_value(area->get_zoom());
        area->assign_db(std::shared_ptr<osmdb::DisplayDB>(new osmdb::DisplayDB(odb)));
        area->zoom_changed.connect([zoomer](int val)
        {
            zoomer->get_adjustment()->set_value(val);
        });
        area->element_clicked.connect([view](std::vector<std::unique_ptr<osm::Element> > const & els)
        {
            view->get_buffer()->set_text(get_els_text(els));
        });
        if (zoom >= 1 && zoom <= 15)
            area->set_zoom(zoom);
        if (lat >= -90 && lat <= 90)
            area->set_latitude(lat);
        if (lon >= -180 && lon <= 180)
            area->set_longitude(lon);

        area->show();

        kit.run(*wnd);
    }
    catch (std::exception& err)
    {
        std::cout << "Error running displayer: " << std::endl << err.what() << std::endl;
        return 1;
    }
}


