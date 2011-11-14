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
#include <memory>
#include "DisplayDB.h"
#include "../projection/projection.h"

namespace display
{

class MapDrawingArea : public Gtk::DrawingArea
{
public:
    MapDrawingArea(BaseObjectType* cobject, Glib::RefPtr<Gtk::Builder> const&);
    void assign_db(std::shared_ptr<DisplayDB> db);
    virtual ~MapDrawingArea();
protected:
    virtual bool on_draw(Cairo::RefPtr<Cairo::Context> const& cr);
    virtual bool on_button_press_event(GdkEventButton* event);
    virtual bool on_button_release_event(GdkEventButton* event);
    virtual bool on_motion_notify_event(GdkEventMotion* event);
    virtual bool on_leave_notify_event(GdkEventCrossing* event);
    virtual bool on_enter_notify_event(GdkEventCrossing* event);
    virtual bool on_scroll_event(GdkEventScroll* event);
    virtual void on_size_allocate(Gtk::Allocation& alloc);
private:
    std::shared_ptr<DisplayDB> db;
    bool pressed;
    double press_x;
    double press_y;
    int zoom;
    std::unique_ptr<proj::MapProjection> proj;
    void get_projection(double x_mov, double y_mov);
    void redraw(Cairo::RefPtr<Cairo::Context> const& cr, double t_x = 0, double t_y = 0);
    void complete_redraw();
    std::vector<Cairo::RefPtr<Cairo::ImageSurface>> surfaces;
    void setup_surfaces();
    int surf_index;
    double get_radius_for_zoom();
    proj::FlatPoint topleft;
    proj::FlatPoint bottomright;
};

} /* namespace display */
#endif /* MAPDRAWINGAREA_H_ */
