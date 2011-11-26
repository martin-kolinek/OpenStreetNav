/*
 * wkb.h
 *
 *  Created on: Nov 26, 2011
 *      Author: martin
 */

#ifndef WKB_H_
#define WKB_H_

#include "../geoelements/geoelements.h"

namespace wkb
{

std::vector<char> point_to_wkb(geo::Point const& p);

}

#endif /* WKB_H_ */
