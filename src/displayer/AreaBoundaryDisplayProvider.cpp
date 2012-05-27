/*
 * AreaBoundaryDisplayProvider.cpp
 *
 *  Created on: May 26, 2012
 *      Author: martin
 */

#include "AreaBoundaryDisplayProvider.h"
#include "PureDisplayLine.h"

namespace display
{

AreaBoundaryDisplayProvider::AreaBoundaryDisplayProvider(std::shared_ptr<DisplayStyle> style):
    style(style)
{

}

AreaBoundaryDisplayProvider::element_range AreaBoundaryDisplayProvider::get_display_elements()
{
    renew_cache();
    return cache;
}

void AreaBoundaryDisplayProvider::renew_cache()
{
    if (cache_ok)
        return;
    cache.clear();
    for (auto it = area.begin(); it != area.end(); ++it)
    {
        if (it->start.is_in_box(top, left, bottom, right) || it->end.is_in_box(top, left, bottom, right))
        {
            cache.push_back(std::shared_ptr<PureDisplayLine>(new PureDisplayLine(it->start, it->end, style)));
        }
    }
    cache_ok = true;
}

void AreaBoundaryDisplayProvider::set_boundary(std::vector<geo::Point> const& ps)
{
    area.clear();
    for (auto it = ps.begin(); it != ps.end(); ++it)
    {
        auto it2 = it;
        ++it2;
        if (it2 == ps.end())
            it2 = ps.begin();
        area.push_back(geo::Edge(*it, *it2));
    }
    cache_ok = false;
}

void AreaBoundaryDisplayProvider::set_bounds(const geo::Point& topleft,
        const geo::Point& bottomright, int)
{
    top = topleft.lat;
    left = topleft.lon;
    right = bottomright.lon;
    bottom = bottomright.lat;
    cache_ok = false;
}

std::vector<std::shared_ptr<Descriptible> > AreaBoundaryDisplayProvider::get_selected(
    const geo::Point& , const geo::Point& , int )
{
    return std::vector<std::shared_ptr<Descriptible> >();
}

double AreaBoundaryDisplayProvider::center_lat()
{
    return 0;
}

double AreaBoundaryDisplayProvider::center_lon()
{
    return 0;
}

int AreaBoundaryDisplayProvider::get_min_zoom()
{
    return 0;
}

int AreaBoundaryDisplayProvider::get_max_zoom()
{
    return 0;
}

AreaBoundaryDisplayProvider::~AreaBoundaryDisplayProvider()
{
}

} /* namespace display */
