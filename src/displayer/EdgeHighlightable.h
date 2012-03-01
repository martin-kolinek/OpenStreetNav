#ifndef EDGE_HIGHLIGHTABLE_H_
#define EDGE_HIGHLIGHTABLE_H_

#include <boost/range.hpp>
#include <boost/range/any_range.hpp>
#include "../elements/osmelements.h"

namespace display
{

class EdgeHighlightable
{
public:
    typedef boost::any_range<osm::Edge, boost::forward_traversal_tag, osm::Edge, size_t> edge_range;
    virtual ~EdgeHighlightable();
    virtual edge_range get_edges() = 0;
};

}

#endif
