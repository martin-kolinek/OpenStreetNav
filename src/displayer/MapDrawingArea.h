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
#include "../projection/projection.h"
#include "DisplayProvider.h"

namespace display
{

class MapDrawingArea : public Gtk::DrawingArea
{
public:
    MapDrawingArea(BaseObjectType* cobject, Glib::RefPtr<Gtk::Builder> const&);
    void add_dp(int priority, std::shared_ptr<DisplayProvider> dp);
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
    std::multimap<int, std::shared_ptr<DisplayProvider> > dps;
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

    std::vector<std::unique_ptr<osm::Element> > get_selected(geo::Point const& p1, geo::Point const& p2, int zoom);
};

} /* namespace display */
#endif /* MAPDRAWINGAREA_H_ */
