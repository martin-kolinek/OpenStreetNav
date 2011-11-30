/*
 * MapDrawingArea.cpp
 *
 *  Created on: Nov 14, 2011
 *      Author: martin
 */

#include "MapDrawingArea.h"
#include <cmath>
#include <iostream>

namespace display
{

MapDrawingArea::MapDrawingArea(BaseObjectType* cobject, const Glib::RefPtr<Gtk::Builder> &):
    Gtk::DrawingArea(cobject),
    lclick_x(-50),
    lclick_y(-50),
    zoom(6),
    tran_x(0),
    tran_y(0)
{
    add_events(Gdk::SCROLL_MASK | Gdk::ENTER_NOTIFY_MASK | Gdk::LEAVE_NOTIFY_MASK | Gdk::BUTTON_PRESS_MASK | Gdk::BUTTON_RELEASE_MASK | Gdk::POINTER_MOTION_MASK);
    setup_surfaces();
}

void MapDrawingArea::assign_db(std::shared_ptr<osmdb::DisplayDB> db)
{
    this->db = db;
}

MapDrawingArea::~MapDrawingArea()
{
}

bool MapDrawingArea::on_draw(const Cairo::RefPtr<Cairo::Context> & cr)
{
    redraw(cr);
    return true;
}

bool MapDrawingArea::on_button_press_event(GdkEventButton* event)
{
    if (event->button == 3)
    {
        pressed = true;
        press_x = event->x;
        press_y = event->y;
    }
    if (event->button == 1)
    {
        double mx = std::max(get_width(), get_height());
        double new_x = ((event->x + (mx - get_width()) / 2.0) * 2 / mx) - 1;
        double new_y = ((event->y + (mx - get_height()) / 2.0) * 2 / mx) - 1;
        lclick_x = new_x;
        lclick_y = new_y;
        auto p1 = proj->unproject(new_x - 0.1, new_y - 0.1);
        auto p2 = proj->unproject(new_x + 0.1, new_y + 0.1);
        auto v = db->get_selected(p1, p2, zoom);
        element_clicked(v);
        complete_redraw();
    }
    return true;
}

bool MapDrawingArea::on_button_release_event(GdkEventButton* event)
{
    if (event->button == 3)
    {
        tran_x = 0;
        tran_y = 0;
        pressed = false;
        lclick_x = -50;
        lclick_y = -50;
        get_projection(event->x - press_x, - event->y + press_y);
        setup_bounds();
        setup_db();
        complete_redraw();
        report_pos();
    }
    return true;
}

bool MapDrawingArea::on_motion_notify_event(GdkEventMotion* event)
{
    if (pressed)
    {
        tran_x = event->x - press_x;
        tran_y = event->y - press_y;
        invalidate();
    }
    return true;
}

bool MapDrawingArea::on_leave_notify_event(GdkEventCrossing*)
{
    return true;
}

bool MapDrawingArea::on_enter_notify_event(GdkEventCrossing*)
{
    return true;
}

bool MapDrawingArea::on_scroll_event(GdkEventScroll* event)
{
    bool scrolled = false;
    switch (event->direction)
    {
        case GDK_SCROLL_UP:
            zoom = std::min(zoom + 1, 20);
            scrolled = true;
            break;
        case GDK_SCROLL_DOWN:
            zoom = std::max(zoom - 1, 1);
            scrolled = true;
            break;
        default:
            break;
    }
    if (scrolled)
    {
        get_projection(0, 0);
        setup_bounds();
        setup_db();
        complete_redraw();
        report_pos();
        zoom_changed(zoom);
    }
    return true;
}

void MapDrawingArea::on_size_allocate(Gtk::Allocation& alloc)
{
    DrawingArea::on_size_allocate(alloc);
    setup_bounds();
    setup_surfaces();
    complete_redraw();
    report_pos();
}

void MapDrawingArea::setup_bounds()
{
    double x1, x2, y1, y2;
    if (get_width() > get_height())
    {
        x1 = -1;
        x2 = 1;
        y1 = (double)(((get_height()))) / (double)(((get_width())));
        y2 = -1;
    }
    else
    {
        x2 = (double)(((get_width()))) / (double)(((get_height())));
        x1 = -x2;
        y1 = 1;
        y2 = -1;
    }
    topleft = proj::FlatPoint(x1, y1);
    bottomright = proj::FlatPoint(x2, y2);
}

void MapDrawingArea::get_projection(double x_mov, double y_mov)
{
    auto mx = std::max(get_width(), get_height());
    auto pos = proj->unproject(-(2 * x_mov) / mx, -(2 * y_mov) / mx);
    proj = std::unique_ptr < proj::MapProjection > (new proj::OrthoProjection(pos, get_radius_for_zoom()));
}

void MapDrawingArea::invalidate()
{
    Glib::RefPtr<Gdk::Window> win = get_window();
    if (win)
    {
        Gdk::Rectangle r(0, 0, get_allocation().get_width(), get_allocation().get_height());
        win->invalidate_rect(r, false);
    }
}

void MapDrawingArea::redraw(const Cairo::RefPtr<Cairo::Context> & cr)
{
    cr->translate(tran_x, tran_y);
    cr->set_source(surface, 0, 0);
    cr->paint();
}

void MapDrawingArea::complete_redraw()
{
    auto cr = Cairo::Context::create(surface);
    auto mx = std::max(surface->get_width(), surface->get_height());
    cr->scale(mx / 2.0, mx / 2.0);
    cr->translate(surface->get_width() / (double)(mx), surface->get_height() / (double)(mx));
    cr->set_source_rgb(0, 0, 0);
    cr->paint();
    cr->set_line_width(0.005);
    cr->set_source_rgb(0.8, 0.8, 0.8);
    for (auto it = db->get_edges().begin(); it != db->get_edges().end(); ++it)
    {
        proj::FlatPoint fps = proj->project(it->start.lat, it->start.lon);
        proj::FlatPoint fpe = proj->project(it->end.lat, it->end.lon);
        cr->move_to(fps.x, -fps.y);
        cr->line_to(fpe.x, -fpe.y);
        cr->stroke();
    }
    for (auto it = db->get_points().begin(); it != db->get_points().end(); ++it)
    {
        proj::FlatPoint fp = proj->project(it->lat, it->lon);
        cr->arc(fp.x, -fp.y, 0.01, 0, 2 * M_PI);
        cr->fill();
    }
    cr->set_source_rgb(0, 1, 0);
    cr->move_to(lclick_x - 0.01, lclick_y - 0.01);
    cr->line_to(lclick_x - 0.01, lclick_y + 0.01);
    cr->line_to(lclick_x + 0.01, lclick_y + 0.01);
    cr->line_to(lclick_x + 0.01, lclick_y - 0.01);
    cr->close_path();
    cr->fill();
    invalidate();
}

void MapDrawingArea::setup_surfaces()
{
    surface = Cairo::ImageSurface::create(Cairo::FORMAT_RGB24, get_width(), get_height());
}

double MapDrawingArea::get_radius_for_zoom()
{
    return (double)(((8 << zoom)));
}

void MapDrawingArea::setup_db()
{
    geo::Point tl = proj->unproject(topleft);
    geo::Point br = proj->unproject(bottomright);
    geo::Point tr = proj->unproject(bottomright.x, topleft.y);
    geo::Point bl = proj->unproject(topleft.x, bottomright.y);
    db->set_bounds(geo::Point(std::max(tl.lat, tr.lat), std::min(tl.lon, bl.lon)), geo::Point(std::min(br.lat, bl.lat), std::max(br.lon, tr.lon)), zoom);
}

void MapDrawingArea::center(double lat, double lon)
{
    proj = std::unique_ptr < proj::MapProjection > (new proj::OrthoProjection(geo::Point(lat, lon), get_radius_for_zoom()));
    setup_bounds();
    setup_db();
    complete_redraw();
    invalidate();
    report_pos();
}

void MapDrawingArea::set_zoom(int z)
{
    zoom = std::max(std::min(z, 20), 1);
    get_projection(0, 0);
    setup_bounds();
    setup_db();
    complete_redraw();
    report_pos();
    zoom_changed(zoom);
}

void MapDrawingArea::report_pos()
{
    auto tl = proj->unproject(topleft);
    auto br = proj->unproject(bottomright);
    std::cout << "LAT: " << br.lat << " - " << tl.lat << " LON: " << tl.lon << " - " << br.lon << " ZOOM: " << zoom << std::endl;
}

} /* namespace display */
