#ifndef ALLWAYLISTER_H_
#define ALLWAYLISTER_H_

#include "../elements/osmelements.h"
#include "../util/sortedcombiterator.h"
#include "OsmDatabase.h"
#include <map>
#include <string>
#include <vector>
#include <boost/range/any_range.hpp>

namespace osmdb
{
class AllWayLister
{
public:
    AllWayLister(OsmDatabase& db, std::multimap<std::string, std::string> const& attributes);
    boost::any_range<osm::Way, boost::single_pass_traversal_tag, osm::Way const&, int> const& get_range();
private:
    boost::any_range<osm::Way, boost::single_pass_traversal_tag, osm::Way const&, int> rng;
    psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, std::string, std::string> > way_attr_crs;
    psql::Cursor<psql::BindTypes<>, psql::RetTypes<int64_t, int64_t, double, double, std::string, std::string> > way_node_attr_crs;
};
}

#endif
