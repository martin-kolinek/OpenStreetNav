#include "Descriptible.h"
#include "../elements/osmelements.h"

namespace display
{

Descriptible::~Descriptible()
{
}

std::vector<std::unique_ptr<osm::WayRegion> > Descriptible::get_regions() const
{
    return std::vector<std::unique_ptr<osm::WayRegion> >();
}

}
