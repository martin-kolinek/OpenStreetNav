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
    pressed(false),
    zoom(6),
    tran_x(0),
    tran_y(0),
    topleft(-1, -1),
    bottomright(1, 1),
    lat(0),
    lon(0)
{
    add_events(Gdk::SCROLL_MASK | Gdk::ENTER_NOTIFY_MASK | Gdk::LEAVE_NOTIFY_MASK | Gdk::BUTTON_PRESS_MASK | Gdk::BUTTON_RELEASE_MASK | Gdk::POINTER_MOTION_MASK);
    setup_projection();
    setup_surfaces();
    setup_bounds();
}

void MapDrawingArea::assign_db(std::shared_ptr<osmdb::DisplayDB> db)
{
    this->db = db;
    lat = db->center_lat();
    lon = db->center_lon();
    after_change();
}

MapDrawingArea::~MapDrawingArea()
{
}

bool MapDrawingArea::on_draw(const Cairo::RefPtr<Cairo::Context> & cr)
{
    cr->translate(tran_x, tran_y);
    cr->set_source(surface, 0, 0);
    cr->paint();
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
        double new_x = event->x;
        double new_y = event->y;
        inverse.transform_point(new_x, new_y);
        auto p1 = proj->unproject(new_x - 0.01, new_y - 0.01);
        auto p2 = proj->unproject(new_x + 0.01, new_y + 0.01);
        auto v = db->get_selected(p1, p2, zoom);
        element_clicked(v);
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
        move_center(press_x - event->x, press_y - event->y);
        after_change();
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
    int z = zoom;
    switch (event->direction)
    {
        case GDK_SCROLL_UP:
            z = set_zoom(zoom + 1);
            break;
        case GDK_SCROLL_DOWN:
            z = set_zoom(zoom - 1);
            break;
        default:
            break;
    }
    if (z != zoom)
    {
        after_change();
        zoom_changed(zoom);
    }
    return true;
}

void MapDrawingArea::on_size_allocate(Gtk::Allocation& alloc)
{
    DrawingArea::on_size_allocate(alloc);
    setup_surfaces();
    setup_bounds();
    after_change();
}

void MapDrawingArea::setup_surfaces()
{
    surface = Cairo::ImageSurface::create(Cairo::FORMAT_RGB24, get_width(), get_height());
}

void MapDrawingArea::setup_bounds()
{
    double x1, x2, y1, y2;
    double scale;
    if (get_width() > get_height())
    {
        x1 = -1;
        x2 = 1;
        y1 = (double)(get_height()) / (double)(get_width());
        y2 = -y1;
        scale = get_width();
    }
    else
    {
        x2 = ((double)get_width()) / (double)(get_height());
        x1 = -x2;
        y1 = 1;
        y2 = -1;
        scale = get_height();
    }
    topleft = proj::FlatPoint(x1, y1);
    bottomright = proj::FlatPoint(x2, y2);
    inverse = Cairo::identity_matrix();
    if (scale > 0)
    {
        inverse.scale(scale / 2.0, scale / 2.0);
    }
    inverse.translate(get_width() / scale, get_height() / scale);
    inverse.scale(1, -1);
    matrix = inverse;
    inverse.invert();
}

void MapDrawingArea::setup_projection()
{
    proj = std::unique_ptr < proj::MapProjection > (new proj::OrthoProjection(geo::Point(lat, lon), get_radius_for_zoom()));
}

void MapDrawingArea::redraw_from_db()
{
    auto cr = Cairo::Context::create(surface);
    cr->transform(matrix);
    cr->set_source_rgb(0, 0, 0);
    cr->paint();
    cr->set_line_width(0.005);
    cr->set_source_rgb(0.8, 0.8, 0.8);
    for (auto it = db->get_edges().begin(); it != db->get_edges().end(); ++it)
    {
        proj::FlatPoint fps = proj->project(it->start.lat, it->start.lon);
        proj::FlatPoint fpe = proj->project(it->end.lat, it->end.lon);
        cr->move_to(fps.x, fps.y);
        cr->line_to(fpe.x, fpe.y);
        cr->stroke();
    }
    for (auto it = db->get_points().begin(); it != db->get_points().end(); ++it)
    {
        proj::FlatPoint fp = proj->project(it->lat, it->lon);
        cr->arc(fp.x, -fp.y, 0.01, 0, 2 * M_PI);
        cr->fill();
    }
    invalidate();
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

void MapDrawingArea::after_change()
{
    setup_projection();
    if (db)
    {
        setup_db();
        redraw_from_db();
    }

}

void MapDrawingArea::move_center(double x, double y)
{
    x += get_width() / 2.0;
    y += get_height() / 2.0;
    inverse.transform_point(x, y);
    auto p = proj->unproject(x, y);
    lat = p.lat;
    lon = p.lon;
}

double MapDrawingArea::get_radius_for_zoom()
{
    return (double)((((((8 << zoom))))));
}

void MapDrawingArea::setup_db()
{
    geo::Point tl = proj->unproject(topleft);
    geo::Point br = proj->unproject(bottomright);
    geo::Point tr = proj->unproject(bottomright.x, topleft.y);
    geo::Point bl = proj->unproject(topleft.x, bottomright.y);
    db->set_bounds(geo::Point(std::max(tl.lat, tr.lat), std::min(tl.lon, bl.lon)), geo::Point(std::min(br.lat, bl.lat), std::max(br.lon, tr.lon)), zoom);
}

int MapDrawingArea::set_zoom(int z)
{
    zoom = std::max(std::min(z, 15), 1);
    after_change();
    zoom_changed(zoom);
    return zoom;
}

void MapDrawingArea::set_latitude(double lat)
{
    this->lat = lat;
}

void MapDrawingArea::set_longitude(double lon)
{
    this->lon = lon;
}

int MapDrawingArea::get_zoom()
{
    return zoom;
}

} /* namespace display */
