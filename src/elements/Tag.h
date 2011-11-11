/*
 * Tag.h
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#ifndef TAG_H_
#define TAG_H_

#include <string>

namespace osm
{

class Tag
{
public:
	Tag();
    Tag(std::string const& key, std::string const& value);
    virtual ~Tag();
    std::string key;
    std::string value;
    bool operator==(Tag const& other) const;
    bool operator!=(Tag const& other) const;
};

} /* namespace osm */
#endif /* TAG_H_ */
