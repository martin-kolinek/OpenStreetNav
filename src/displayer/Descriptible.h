#ifndef DESCRIPTIBLE_H_
#define DESCRIPTIBLE_H_

#include <boost/property_tree/ptree.hpp>
#include "../elements/HashElementContainer.h"

namespace display
{

class Descriptible
{
public:
    virtual boost::property_tree::ptree get_description() const = 0;
    virtual std::shared_ptr<osm::HashElementContainer> get_highlighted() const;
    virtual ~Descriptible();
};

}
#endif
