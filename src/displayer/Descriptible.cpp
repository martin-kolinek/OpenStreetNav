#include "Descriptible.h"
#include "../elements/osmelements.h"

namespace display
{
std::shared_ptr<osm::HashElementContainer> Descriptible::get_highlighted() const
{
    return std::shared_ptr<osm::HashElementContainer>(new osm::HashElementContainer());
}

Descriptible::~Descriptible()
{
}

}
