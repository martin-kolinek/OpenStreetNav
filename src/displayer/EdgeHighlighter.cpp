#include "EdgeHighlighter.h"
#include <memory>
#include "DisplayLine.h"
#include <boost/range/adaptors.hpp>
#include <boost/bind.hpp>

namespace display
{
EdgeHighlighter::EdgeHighlighter(EdgeHighlightable& src, std::vector<osm::WayRegion> const& v, LineDisplayStyle const& style):
    src(src),
    style(style),
    renew_cache(true)
{
    for (auto it = v.begin(); it != v.end(); ++it)
    {
        highlight.insert(std::make_pair(osm::Way(it->get_way().id), *it));
    }
}

bool highlight_edge_filter(osm::Edge const& e, std::map<osm::Way, osm::WayRegion, osm::LtByID> const& highlight)
{
    auto it = highlight.find(e.way);
    if (it == highlight.end())
        return false;
    return it->second.intersects(e);
}

std::shared_ptr<DisplayElement> elem_from_edge(osm::Edge const& e, LineDisplayStyle const& s)
{
    return std::shared_ptr<DisplayElement>(new DisplayLine(e.start_node.position, e.end_node.position,
                                           std::unique_ptr<LineDisplayStyle>(new LineDisplayStyle(s))));
}

EdgeHighlighter::element_range EdgeHighlighter::get_display_elements()
{
    if (!renew_cache)
        return cache;
    auto rng = src.get_edges() |
               boost::adaptors::filtered([&](osm::Edge const & e)
    {
        return highlight_edge_filter(e, highlight);
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
std::vector<std::unique_ptr<osm::Element> > EdgeHighlighter::get_selected(geo::Point const& , geo::Point const& , int )
{
    return std::vector<std::unique_ptr<osm::Element> >();
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

}
