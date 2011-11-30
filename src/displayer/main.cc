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

int main(int argc, char** argv)
{
    /*if (argc != 2)
    {
        std::cout << "unknown command line option only database name accepted" << std::endl;
        return 1;
    }*/
    Gtk::Main kit(argc, argv);
    Glib::RefPtr<Gtk::Builder> bldr = Gtk::Builder::create_from_file("build/bin/display.glade");
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

    psql::Database pdb("");
    psql::execute_sql(pdb, "SET search_path TO slovakia_osm, public");
    osmdb::OsmDatabase odb(pdb);
    area->assign_db(std::shared_ptr<osmdb::DisplayDB>(new osmdb::DisplayDB(odb)));
    area->center(48.143, 17.109);
    area->zoom_changed.connect([zoomer](int val)
    {
        zoomer->get_adjustment()->set_value(val);
    });
    area->element_clicked.connect([view](std::vector<std::unique_ptr<osm::Element> > const & els)
    {
        view->get_buffer()->set_text(get_els_text(els));
    });

    conn.update();
    zoomer->get_adjustment()->set_value(6);
    area->show();

    kit.run(*wnd);
}


