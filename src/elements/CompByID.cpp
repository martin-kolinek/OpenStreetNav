/*
 * CompByID.cpp
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#include "CompByID.h"
#include "../util.h"

namespace osm
{

bool EqByID::operator ()(const Element& a, const Element& b) const
{
    return a.get_type() == b.get_type() && a.get_id() == b.get_id();
}

bool LtByID::operator ()(const Element& a, const Element& b) const
{
    if (a.get_type() < b.get_type())
        return true;
    if (a.get_type() > b.get_type())
        return false;
    return a.get_id() < b.get_id();
}

bool GtByID::operator ()(const Element& a, const Element& b) const
{
    return util::greater_than_impl<LtByID, EqByID>(a, b);
}

bool GeByID::operator ()(const Element& a, const Element& b) const
{
    return util::greater_eq_impl<LtByID>(a, b);
}

bool LeByID::operator ()(const Element& a, const Element& b) const
{
    return util::less_eq_impl<LtByID, EqByID>(a, b);
}

bool NeByID::operator ()(const Element& a, const Element& b) const
{
    return util::not_eq_impl<EqByID>(a, b);
}

}

/* namespace osm */
