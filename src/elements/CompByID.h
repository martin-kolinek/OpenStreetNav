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
	bool operator()(Element const& a, Element const& b);

};

class NeByID
{
public:
	bool operator()(Element const& a, Element const& b);

};

class LtByID
{
public:
	bool operator()(Element const& a, Element const& b);
};

class GtByID
{
public:
	bool operator()(Element const& a, Element const& b);
};

class GeByID
{
public:
	bool operator()(Element const& a, Element const& b);
};

class LeByID
{
public:
	bool operator()(Element const& a, Element const& b);
};

template<typename A, typename B>
class DerefEqByID
{
public:
	bool operator()(A const& a, B const& b)
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
