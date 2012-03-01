#include "WayRegion.h"
#include "../util/util.h"

namespace osm
{
WayRegion::WayRegion(Way const& w):
    w(w)
{
    regions.push_back(std::make_pair(0, w.nodes.size() - 1));
}

bool WayRegion::intersects(Edge const& e) const
{
    if (e.way.id != w.id)
        return false;
    bool in_edge = false;
    bool in_region = false;
    auto it = regions.begin();
    for (unsigned int i = 0; i < w.nodes.size(); ++i)
    {
        if (w.nodes[i].id == e.start_node.id)
            in_edge = true;
        if (w.nodes[i].id == e.end_node.id)
            in_edge = false;
        if (i == it->first)
            in_region = true;
        if (i == it->second)
        {
            in_region = false;
            ++it;
        }
        if (in_region && in_edge)
            return true;
    }
    return false;
}

bool WayRegion::operator==(WayRegion const& other) const
{
    return other.w.id == w.id;
}
bool WayRegion::operator!=(WayRegion const& other) const
{
    return util::not_eq_impl(*this, other);
}
bool WayRegion::operator<=(WayRegion const& other) const
{
    return util::less_eq_impl(*this, other);
}
bool WayRegion::operator>=(WayRegion const& other) const
{
    return util::greater_eq_impl(*this, other);
}
bool WayRegion::operator>(WayRegion const& other) const
{
    return util::greater_than_impl(*this, other);
}
bool WayRegion::operator<(WayRegion const& other) const
{
    return w.id < other.w.id;
}

osm::Way const& WayRegion::get_way() const
{
    return w;
}

}
