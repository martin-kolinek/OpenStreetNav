/*
 * DisplayProvider.h
 *
 *  Created on: Jan 5, 2012
 *      Author: martin
 */

#ifndef DISPLAYPROVIDER_H_
#define DISPLAYPROVIDER_H_

#include <vector>
#include "../geoelements/geoelements.h"
#include "../elements/osmelements.h"
#include "DisplayElement.h"

namespace display
{

class DisplayProvider
{
public:
    virtual std::vector<std::unique_ptr<display::DisplayElement> > const& get_display_elements() = 0;
    virtual void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
    virtual std::vector<std::unique_ptr<osm::Element> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
    virtual double center_lat() = 0;
    virtual double center_lon() = 0;
    virtual ~DisplayProvider();
};

} /* namespace display */
#endif /* DISPLAYPROVIDER_H_ */
