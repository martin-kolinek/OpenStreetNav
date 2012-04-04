#include "WayRegion.h"
#include "../util/util.h"
#include "Edge.h"
#include <boost/range/algorithm.hpp>

namespace osm
{
WayRegion::WayRegion(Way const& w):
    w(w)
{
    if (w.nodes.begin() == w.nodes.end())
        return;
    int st = w.nodes.begin()->first;
    int last = (--w.nodes.end())->first;
    regions.push_back(std::make_pair(st, last));
}

bool WayRegion::intersects(SeqEdge const& e) const
{
    if (e.get_way().id != w.id)
        return false;
    for (unsigned int i = 0; i < regions.size(); ++i)
    {
        int st = regions[i].first;
        int en = regions[i].second;
        if (e.get_end_seq_no() > st && e.get_start_seq_no() < en)
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

int WayRegion::get_first_seq_no() const
{
    return boost::min_element(regions, util::get_tuple_less<0>())->first;
}
int WayRegion::get_last_seq_no() const
{
    return boost::max_element(regions, util::get_tuple_less<1>())->second;
}

}
