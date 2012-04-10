#include "Descriptible.h"
#include "../elements/osmelements.h"

namespace display
{
std::vector<std::shared_ptr<osm::HashElementContainer> > Descriptible::get_highlighted() const
{
    return std::vector<std::shared_ptr<osm::HashElementContainer> >();
}

Descriptible::~Descriptible()
{
}

}
