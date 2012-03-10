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

/**
 * \class MapDrawingArea
 * Responsible for drawing data provided by DisplayProviders.
 */
class MapDrawingArea : public Gtk::DrawingArea
{
public:
    /**
     * Constructs a MapDrawingArea
     * @param cobject
     * @param
     */
    MapDrawingArea(BaseObjectType* cobject, Glib::RefPtr<Gtk::Builder> const&);
    /**
     * Adds a DisplayProvider to considered DisplayProviders. Priority determines order. Lower priorities go first
     * and DisplayElements are not drawn over.
     * @param priority
     * @param dp
     */
    void add_dp(int priority, std::shared_ptr<DisplayProvider> dp);
    /**
     * Sets latitude to look at.
     * @param lat
     */
    void set_latitude(double lat);
    /**
     * Sets longitude to look at.
     * @param lon
     */
    void set_longitude(double lon);
    /**
     * Sets zoom level.
     * @param zoom
     * @return zoom level afterwards
     */
    int set_zoom(int zoom);
    /**
     *
     * @return current zoom level
     */
    int get_zoom();
    /**
     * This signal gets signalled when zoom level is changed. The parameter is new zoom level.
     */
    boost::signal<void (int)> zoom_changed;
    /**
     * This signal gets signalled when element is clicked. The parameter is a vector of displayed elements
     * that are in the vicinity of point clicked.
     */
    boost::signal<void (std::vector<std::unique_ptr<Descriptible> > const& elem)> element_clicked;

    void refresh();
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

    std::vector<std::unique_ptr<Descriptible> > get_selected(geo::Point const& p1, geo::Point const& p2, int zoom);
};

} /* namespace display */
#endif /* MAPDRAWINGAREA_H_ */
