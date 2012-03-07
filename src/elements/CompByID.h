/*
 * EqByID.h
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#ifndef COMPBYID_H_
#define COMPBYID_H_

#include "Element.h"

namespace osm
{

class EqByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;

};

class NeByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;

};

class LtByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;
};

class GtByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;
};

class GeByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;
};

class LeByID
{
public:
    typedef bool result_type;
    bool operator()(Element const& a, Element const& b) const;
};

template<typename A, typename B>
class DerefEqByID
{
public:
    typedef bool result_type;
    bool operator()(A const& a, B const& b) const
    {
        return EqByID()(*a, *b);
    }
};

template<typename A, typename B>
DerefEqByID<A, B> deref_eq_by_id(A const& a, B const& b)
{
    return DerefEqByID<A, B>();
}

} /* namespace osm */
#endif /* COMPBYID_H_ */
