// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#include "way-pimpl.hxx"

// way_pimpl
//

void way_pimpl::
pre ()
{
	tags.clear();
	nodes.clear();
}

void way_pimpl::
tag (const std::pair<std::string, std::string>& tag)
{
	tags.insert(tag);
}

void way_pimpl::
nd (int64_t nd)
{
	nodes.push_back(osm::Node(nd));
}

void way_pimpl::
id (long long id)
{
	_id=id;
}

osm::Way way_pimpl::
post_way ()
{
	osm::Way ret(_id);
	ret.tags=tags;
	ret.nodes=nodes;
	return ret;
}

