#ifndef TAG_H_
#define TAG_H_

#include <boost/optional.hpp>
#include <boost/range/algorithm.hpp>
#include <string>
#include "../util/tuple_sub.h"

namespace osm
{
typedef std::pair<std::string, std::string> Tag;

template<typename Rng>
boost::optional<Tag> get_tag(Rng const& r, std::string const& key)
{
    auto it = boost::range::find_if(r,
                                    [&key](osm::Tag const & t)
    {
        return t.first == key;
    });
    if (it == r.end())
        return boost::optional<Tag>();
    return *it;
}
}

#endif
