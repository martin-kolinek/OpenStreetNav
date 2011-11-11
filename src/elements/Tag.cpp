/*
 * Tag.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "Tag.h"

namespace osm
{

Tag::Tag()
{
}

Tag::Tag(std::string const& key, std::string const& value):
    key(key),
    value(value)
{
}

Tag::~Tag()
{
}

bool Tag::operator==(Tag const& other) const
{
    return key == other.key && value == other.value;
}

bool Tag::operator!=(Tag const& other) const
{
    return !(*this == other);
}

}
