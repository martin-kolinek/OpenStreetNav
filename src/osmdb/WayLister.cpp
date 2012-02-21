/*
 * WayLister.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include "WayLister.h"
#include <boost/property_tree/xml_parser.hpp>
#include <boost/range.hpp>
#include <boost/range/join.hpp>
#include "../sqllib/sqllib.h"
#include "../util/RowDataDeserializer.h"
#include <functional>
#include <cmath>
#include "../util/func.h"

namespace osmdb
{

void test_changed(int64_t , int64_t , double , double , int64_t , const std::string & , const std::string & , int )
{
}

WayLister::WayLister(OsmDatabase& db, const std::multimap<std::string,std::string> & attributes, unsigned int fetch_size)
:db(db), done(false), fetch_size(fetch_size)
{
	boost::property_tree::ptree ptree = get_entries(attributes);
	get_way_descr = psql::Cursor < psql::BindTypes<>, psql::RetTypes<int64_t,int64_t,double,double,int64_t,std::string,std::string,int> > (db.get_db(), "wayred_crs", sqllib::get_wayreduction_select(ptree, db.get_db()));
	get_way_descr.open();
}

const std::map<osm::Way,std::multimap<osm::Node,osm::Way,osm::LtByID> ,osm::LtByID> & WayLister::get_current_connected_ways() const
{
	return current_connected_ways;
}

void WayLister::next()
{
	current_connected_ways.clear();
	get_way_descr.fetch(fetch_size);
	if(get_way_descr.get_buffer().size() < fetch_size){
		done = true;
	}

    //boost::join behaves strangely if given non const reference as first argument and
    //const reference as second argument in boost 1.46
    std::vector<std::tuple<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > const& v = rest; 
    
	auto coll = boost::join(v, get_way_descr.get_buffer());
	auto it = util::deserialize_collection(
			coll.begin(),
			coll.end(),
			done,
			util::bind1st(&WayLister::way_changed, this),
			util::bind1st(&WayLister::node_changed, this),
			util::bind1st(&WayLister::empty, this),
			util::bind1st(&WayLister::empty, this),
			util::bind1st(&WayLister::cross_way_changed, this),
			util::bind1st(&WayLister::attr_changed, this),
			util::bind1st(&WayLister::attr_changed, this),
			util::bind1st(&WayLister::empty, this));

	std::vector<std::tuple<int64_t, int64_t, double, double, int64_t, std::string, std::string, int> > new_rest(coll.end()-it);

	std::copy(it, coll.end(), new_rest.begin());

	rest=std::move(new_rest);
}

void WayLister::reset()
{
	get_way_descr.close();
	get_way_descr.open();
}

bool WayLister::end()
{
	return done;
}

boost::property_tree::ptree WayLister::get_entries(const std::multimap<std::string,std::string> & attributes)
{
	boost::property_tree::ptree ret;
	boost::property_tree::ptree entries;
	for(auto it = attributes.begin();it != attributes.end();++it){
		boost::property_tree::ptree entry;
		boost::property_tree::ptree kv;
		kv.put("key", it->first);
		kv.put("value", it->second);
		entry.put_child("elements.el", kv);
		entries.add_child("entry", entry);
	}
	ret.add_child("entries", entries);
	return ret;
}

WayLister::~WayLister()
{
}

void WayLister::empty(int64_t, int64_t , double , double , int64_t, std::string const&, std::string const&, int)
{
}

void WayLister::attr_changed(int64_t, int64_t, double, double, int64_t, const std::string & k, const std::string & v, int)
{
	if(key!=k || val!=v)
	{
		last_cross_way.tags.insert(osm::Tag(k, v));
		key=k;
		val=v;
	}
}

void WayLister::cross_way_changed(int64_t, int64_t , double , double, int64_t cwid, const std::string &, const std::string &, int)
{
	cross_ways.push_back(last_cross_way);
	last_cross_way=osm::Way(cwid);
}

void WayLister::node_changed(int64_t, int64_t nid, double lon, double lat, int64_t , const std::string & , const std::string & , int )
{
	for(unsigned int j = 0;j < cross_ways.size();++j)
		conn_ways_for_way.insert(std::pair<osm::Node,osm::Way>(last_node, cross_ways[j]));
	last_node = osm::Node(nid, lat, lon);
	cross_ways.clear();
}

void WayLister::way_changed(int64_t wid, int64_t , double , double , int64_t , const std::string & , const std::string & , int )
{
	current_connected_ways.insert(std::make_pair(way, conn_ways_for_way));
	way = osm::Way(wid);
	conn_ways_for_way.clear();
}

} /* namespace osmdb */
