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
#include "ObjectType.h"
#include "Tag.h"
#include "../displayer/Descriptible.h"

namespace osmdb
{

class PropertiesSelection;
class ElementImporter;

}

namespace osm
{

/**
 * \class Element
 * Represents an element in OpenStreetMap data.
 */
class Element : public display::Descriptible
{
public:
    virtual ~Element();
    /**
     *
     * @return a description of this element (used for displaying)
     */
    virtual boost::property_tree::ptree get_description() const = 0;
    /**
     *
     * @return the type of this element
     */
    virtual osm::ObjectType get_type() const = 0;
    /**
     * Fill attributes and possibly other information from db
     * @param db
     */
    virtual void fill(osmdb::PropertiesSelection& db) = 0;
    /**
     * Execute the needed method of db to insert this element as member into relation
     * @param db to use when inserting
     * @param parentid relation id
     * @param role role to use when inserting
     */
    virtual void add_to_relation(osmdb::ElementImporter& db, int64_t parentid, std::string const& role) = 0;
    /**
     *
     * @return id of this element
     */
    virtual int64_t get_id() const = 0;
};

} /* namespace osm */
#endif /* ELEMENT_H_ */
