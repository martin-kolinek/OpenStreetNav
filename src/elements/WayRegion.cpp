#include "WayRegion.h"
#include "../util/util.h"
#include "Edge.h"

namespace osm
{
WayRegion::WayRegion(Way const& w):
    w(w)
{
    regions.push_back(std::make_pair(0, w.nodes.size() - 1));
}

bool WayRegion::contains(SeqEdge const& e) const
{
    if (e.get_way().id != w.id)
        return false;
    for (unsigned int i = 0; i < regions.size(); ++i)
    {
        int st = regions[i].first;
        int en = regions[i].second;
        if (e.get_start_seq_no() >= st && e.get_end_seq_no() <= en)
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
