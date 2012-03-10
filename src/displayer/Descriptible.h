#ifndef DESCRIPTIBLE_H_
#define DESCRIPTIBLE_H_

#include <boost/property_tree/ptree.hpp>

namespace osm
{
class WayRegion;
}

namespace display
{

class Descriptible
{
public:
    virtual boost::property_tree::ptree get_description() const = 0;
    virtual std::vector<std::unique_ptr<osm::WayRegion> > get_regions() const;
    virtual ~Descriptible();
};

}
#endif
