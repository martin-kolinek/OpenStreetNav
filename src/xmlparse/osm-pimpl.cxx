// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#include "osm-pimpl.hxx"

// osm_pimpl
//

void osm_pimpl::
pre ()
{
}

void osm_pimpl::
node (const osm::Node& node)
{
	node_handler(node);
}

void osm_pimpl::
way (const osm::Way& way)
{
	way_handler(way);
}

void osm_pimpl::
relation (const osm::Relation& relation)
{
	relation_handler(relation);
}

void osm_pimpl::
post_osm ()
{
}

