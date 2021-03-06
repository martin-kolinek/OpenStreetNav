// Not copyrighted - public domain.
//
// This sample parser implementation was generated by CodeSynthesis XSD,
// an XML Schema to C++ data binding compiler. You may use it in your
// programs without any restrictions.
//

#ifndef OSM_PIMPL_HXX
#define OSM_PIMPL_HXX

#include "osm-pskel.hxx"
#include <functional>
#include "../elements/osmelements.h"

class osm_pimpl: public virtual osm_pskel
{
public:

	std::function<void (osm::Node const&)> node_handler;
	std::function<void (osm::Way const&)> way_handler;
	std::function<void (osm::Relation const&)> relation_handler;
	std::function<void ()> progress_handler;

	virtual void
	pre ();

	virtual void
	node (const osm::Node&);

	virtual void
	way (const osm::Way&);

	virtual void relation(osm::Relation const&);

	virtual void
	post_osm ();
};

#endif // OSM_PIMPL_HXX
