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
/**
 * \class DisplayProvider
 * Interface responsible for supplying what to display.
 */
class DisplayProvider
{
public:
    /**
     *
     * @return the DisplayElements in current view.
     */
    virtual std::vector<std::unique_ptr<display::DisplayElement> > const& get_display_elements() = 0;
    /**
     * Change current view to position bounded by topleft and bottomright using zoomlevel zoom
     * @param topleft
     * @param bottomright
     * @param zoom
     */
    virtual void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
    /**
     * Retrieve osm::Elements in box specified by topleft and bottomright when current zoom level is zoom
     * @param topleft
     * @param bottomright
     * @param zoom
     * @return displayed osm::Elements in box
     */
    virtual std::vector<std::unique_ptr<osm::Element> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
    /**
     *
     * @return center latitude of all data this DisplayProvider can get
     */
    virtual double center_lat() = 0;
    /**
     *
     * @return center longitude of all data this DisplayProvider can get
     */
    virtual double center_lon() = 0;
    virtual ~DisplayProvider();
};

} /* namespace display */
#endif /* DISPLAYPROVIDER_H_ */
