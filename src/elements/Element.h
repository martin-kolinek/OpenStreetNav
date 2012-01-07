/*
 * Element.h
 *
 *  Created on: Nov 29, 2011
 *      Author: martin
 */

#ifndef OSMELEMENT_H_
#define OSMELEMENT_H_

#include <string>
#include <vector>
#include <stdint.h>
#include <boost/property_tree/ptree.hpp>
#include "ObjectType.h"

namespace osmdb
{

class PropertiesSelection;
class ElementImporter;

}

namespace osm
{

typedef std::pair<std::string, std::string> Tag;

class Element
{
public:
    virtual ~Element();
    virtual boost::property_tree::ptree get_description() = 0;
    virtual osm::ObjectType get_type() const = 0;
    virtual bool operator==(Element const& e) const = 0;
    virtual bool operator!=(Element const& e) const;
    virtual void fill(osmdb::PropertiesSelection& db) = 0;
    virtual void add_to_relation(osmdb::ElementImporter& db, int64_t parentid, std::string const& role) = 0;
};

} /* namespace osm */
#endif /* ELEMENT_H_ */
