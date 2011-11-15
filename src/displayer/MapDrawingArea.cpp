/*
 * MapDrawingArea.cpp
 *
 *  Created on: Nov 14, 2011
 *      Author: martin
 */

#include "MapDrawingArea.h"
#include <cmath>

namespace display
{

MapDrawingArea::MapDrawingArea(BaseObjectType* cobject, const Glib::RefPtr<Gtk::Builder> &):
    Gtk::DrawingArea(cobject)
{
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
    return true;
}

bool MapDrawingArea::on_button_release_event(GdkEventButton* event)
{
    if (event->button == 3)
    {
        pressed = false;
        get_projection(event->x - press_x, event->y - press_y);
        geo::Point tl = proj->unproject(topleft);
        geo::Point br = proj->unproject(bottomright);
        geo::Point tr = proj->unproject(bottomright.x, topleft.y);
        geo::Point bl = proj->unproject(topleft.x, bottomright.y);
        db->set_bounds(geo::Point(std::max(tl.lat, tr.lat), std::min(tl.lon, bl.lon)), geo::Point(std::min(br.lat, bl.lat), std::max(br.lon, tr.lon)), zoom);
        complete_redraw();
    }
    return true;
}

bool MapDrawingArea::on_motion_notify_event(GdkEventMotion*)
{
    if (pressed)
    {
        //TODO do some translated drawing
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
    switch (event->direction)
    {
        case GDK_SCROLL_UP:
            zoom = std::min(zoom + 1, 20);
            break;
        case GDK_SCROLL_DOWN:
            zoom = std::max(zoom - 1, 1);
            break;
        default:
            break;
    }
    return true;
}

void MapDrawingArea::on_size_allocate(Gtk::Allocation& alloc)
{
    double x1, x2, y1, y2;
    if (alloc.get_width() > alloc.get_height())
    {
        x1 = -1;
        x2 = 1;
        y1 = (double)(alloc.get_height()) / (double)(alloc.get_width());
        y2 = -1;
    }
    else
    {
        x2 = (double)(alloc.get_width()) / (double)(alloc.get_height());
        x1 = -x2;
        y1 = 1;
        y2 = -1;
    }
    topleft = proj::FlatPoint(x1, y1);
    bottomright = proj::FlatPoint(x2, y2);
    surfaces.clear();
    setup_surfaces();
    complete_redraw();
}

void MapDrawingArea::get_projection(double x_mov, double y_mov)
{
    auto pos = proj->unproject(x_mov, y_mov);
    proj = std::unique_ptr < proj::MapProjection > (new proj::OrthoProjection(pos, get_radius_for_zoom()));
}

void MapDrawingArea::redraw(const Cairo::RefPtr<Cairo::Context> & cr, double t_x, double t_y)
{
    cr->translate(t_x, t_y);
    cr->set_source(surfaces[surf_index], 0, 0);
    cr->paint();
}

void MapDrawingArea::complete_redraw()
{
    int to_draw = (surf_index + 1) % 2;
    auto cr = Cairo::Context::create(surfaces[to_draw]);
    for (auto it = db->get_edges().begin(); it != db->get_edges().end(); ++it)
    {
        auto it2 = db->get_nodes().find(it->start_node);
        if (it2 == db->get_nodes().end())
            assert(false);
        osm::Node st = it2->second;
        it2 = db->get_nodes().find(it->end_node);
        if (it2 == db->get_nodes().end())
            assert(false);
        osm::Node en = it2->second;
        proj::FlatPoint fps = proj->project(st.lat, st.lon);
        proj::FlatPoint fpe = proj->project(en.lat, en.lon);
        cr->move_to(fps.x, fps.y);
        cr->line_to(fpe.x, fpe.y);
    }
    cr->stroke();
}

void MapDrawingArea::setup_surfaces()
{
    surfaces.clear();
    surfaces.push_back(Cairo::ImageSurface::create(Cairo::FORMAT_RGB24, get_width(), get_height()));
    surfaces.push_back(Cairo::ImageSurface::create(Cairo::FORMAT_RGB24, get_width(), get_height()));
    surf_index = 0;
}

double MapDrawingArea::get_radius_for_zoom()
{
    return (double)(8 << zoom);
}

} /* namespace display */
