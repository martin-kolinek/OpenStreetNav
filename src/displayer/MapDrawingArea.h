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
    void center(double lat, double lon);
    void set_zoom(int zoom);
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
    double lclick_x;
    double lclick_y;
    int zoom;
    double tran_x, tran_y;
    std::unique_ptr<proj::MapProjection> proj;
    void get_projection(double x_mov, double y_mov);
    void redraw(Cairo::RefPtr<Cairo::Context> const& cr);
    void complete_redraw();
    Cairo::RefPtr<Cairo::ImageSurface> surface;
    void setup_surfaces();
    double get_radius_for_zoom();
    void setup_db();
    void invalidate();
    void register_signals();
    void setup_bounds();
    proj::FlatPoint topleft;
    proj::FlatPoint bottomright;
    void report_pos();
};

} /* namespace display */
#endif /* MAPDRAWINGAREA_H_ */
