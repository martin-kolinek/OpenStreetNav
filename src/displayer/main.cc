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
#include "DisplayDB.h"
#include <memory>

int main(int argc, char** argv)
{
	if(argc!=2)
	{
		std::cout<<"unknown command line option only database name accepted"<<std::endl;
		return 1;
	}
	Gtk::Main kit(argc, argv);
	Glib::RefPtr<Gtk::Builder> bldr=Gtk::Builder::create_from_file("display.glade");
	Gtk::Window* wnd=0;
	bldr->get_widget("window1", wnd);
	display::MapDrawingArea* area=0;
	bldr->get_widget_derived("drawingarea", area);
	area->assign_db(std::shared_ptr<display::DisplayDB>(new display::DisplayDB(argv[0])));
	kit.run(*wnd);
}

