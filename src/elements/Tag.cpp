/*
 * Tag.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Tag.h"

namespace osm
{

Tag::Tag(std::string const& key, std::string const& value):
    key(key),
    value(value)
{
}

Tag::~Tag()
{
}

}
