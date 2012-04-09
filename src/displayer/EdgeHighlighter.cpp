#include "EdgeHighlighter.h"
#include <memory>
#include "DisplayLine.h"
#include <boost/range/adaptors.hpp>
#include <boost/bind.hpp>

namespace display
{
EdgeHighlighter::EdgeHighlighter(DisplayProvider& src, std::unique_ptr<DisplayStyleChanger> && style):
    src(src),
    style(std::move(style)),
    renew_cache(true)
{
}

bool highlight_edge_filter(osm::Edge const& e, std::map<osm::Way, osm::WayRegion, osm::LtByID> const& highlight)
{
    auto it = highlight.find(e.get_way());
    if (it == highlight.end())
        return false;
    return it->second.intersects(e);
}

std::shared_ptr<DisplayElement> elem_from_edge(std::shared_ptr<DisplayElement> const& de, std::unique_ptr<DisplayStyleChanger> const& s)
{
    return std::shared_ptr<DisplayElement>(new DisplayLine(de->get_edge(),
                                           std::shared_ptr<DisplayStyle>(de->get_style().accept(*s))));
}

EdgeHighlighter::element_range EdgeHighlighter::get_display_elements()
{
    if (!renew_cache)
        return cache;
    auto rng = src.get_display_elements() |
               boost::adaptors::filtered([&](std::shared_ptr<DisplayElement> const & ptr)
    {
        return highlight_edge_filter(ptr->get_edge(), highlight);
    });
    cache.clear();
    for (auto it = rng.begin(); it != rng.end(); ++it)
    {
        cache.push_back(elem_from_edge(*it, style));
    }
    renew_cache = false;
    return cache;

}
void EdgeHighlighter::set_bounds(geo::Point const& , geo::Point const& , int )
{
    renew_cache = true;
}
std::vector<std::unique_ptr<Descriptible> > EdgeHighlighter::get_selected(geo::Point const& , geo::Point const& , int )
{
    return std::vector<std::unique_ptr<Descriptible> >();
}
double EdgeHighlighter::center_lat()
{
    return 0;
}
double EdgeHighlighter::center_lon()
{
    return 0;
}
EdgeHighlighter::~EdgeHighlighter()
{
}

void EdgeHighlighter::add_descriptible(Descriptible const& desc)
{
    auto v = desc.get_regions();
    for (auto it = v.begin(); it != v.end(); ++it)
        highlight.insert(std::make_pair(osm::Way((*it)->get_way().id), **it));
    renew_cache = true;
}

void EdgeHighlighter::add_way_region(osm::WayRegion const& reg)
{
    highlight.insert(std::make_pair(reg.get_way(), reg));
    renew_cache = true;
}

void EdgeHighlighter::clear()
{
    highlight.clear();
    renew_cache = true;
}

int EdgeHighlighter::get_min_zoom()
{
    return 0;
}

int EdgeHighlighter::get_max_zoom()
{
    return 0;
}

}
