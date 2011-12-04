/*
 * MapDrawingArea.h
 *
 *  Created on: Nov 14, 2011
 *      Author: martin
 */

#ifndef MAPDRAWINGAREA_H_
#define MAPDRAWINGAREA_H_

#include <gtkmm/drawingarea.h>
#include <gtkmm/builder.h>
#include <boost/signal.hpp>
#include <memory>
#include "../osmdb/osmdb.h"
#include "../projection/projection.h"

namespace display
{

class MapDrawingArea : public Gtk::DrawingArea
{
public:
    MapDrawingArea(BaseObjectType* cobject, Glib::RefPtr<Gtk::Builder> const&);
    void assign_db(std::shared_ptr<osmdb::DisplayDB> db);
    void set_latitude(double lat);
    void set_longitude(double lon);
    int set_zoom(int zoom);
    int get_zoom();
    boost::signal<void (int)> zoom_changed;
    boost::signal<void (std::vector<std::unique_ptr<osm::Element> > const& elem)> element_clicked;
    virtual ~MapDrawingArea();
protected:
    bool on_draw(Cairo::RefPtr<Cairo::Context> const& cr);
    bool on_button_press_event(GdkEventButton* event);
    bool on_button_release_event(GdkEventButton* event);
    bool on_motion_notify_event(GdkEventMotion* event);
    bool on_leave_notify_event(GdkEventCrossing* event);
    bool on_enter_notify_event(GdkEventCrossing* event);
    bool on_scroll_event(GdkEventScroll* event);
    void on_size_allocate(Gtk::Allocation& alloc);
private:
    std::shared_ptr<osmdb::DisplayDB> db;
    bool pressed;
    double press_x;
    double press_y;
    int zoom;
    double tran_x, tran_y;
    std::unique_ptr<proj::MapProjection> proj;
    Cairo::RefPtr<Cairo::ImageSurface> surface;
    proj::FlatPoint topleft;
    proj::FlatPoint bottomright;
    double lat;
    double lon;
    Cairo::Matrix matrix;
    Cairo::Matrix inverse;

    void move_center(double x, double y);
    void setup_surfaces();

    void after_change();
    void setup_projection();
    void setup_bounds();
    void setup_db();

    void redraw_from_db();
    void invalidate();

    double get_radius_for_zoom();
};

} /* namespace display */
#endif /* MAPDRAWINGAREA_H_ */
