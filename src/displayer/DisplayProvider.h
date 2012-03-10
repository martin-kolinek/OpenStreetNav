/*
 * DisplayProvider.h
 *
 *  Created on: Jan 5, 2012
 *      Author: martin
 */

#ifndef DISPLAYPROVIDER_H_
#define DISPLAYPROVIDER_H_

#include <boost/range.hpp>
#include <boost/range/any_range.hpp>
#include <vector>
#include "../geoelements/geoelements.h"
#include "../elements/osmelements.h"
#include "DisplayElement.h"
#include <memory>

namespace display
{
/**
 * \class DisplayProvider
 * Interface responsible for supplying what to display.
 */
class DisplayProvider
{
public:
    typedef boost::any_range<std::shared_ptr<display::DisplayElement> const, boost::forward_traversal_tag, std::shared_ptr<display::DisplayElement> const, size_t> element_range;
    /**
     *
     * @return the DisplayElements in current view.
     */
    virtual element_range get_display_elements() = 0;
    /**
     * Change current view to position bounded by topleft and bottomright using zoomlevel zoom
     * @param topleft
     * @param bottomright
     * @param zoom
     */
    virtual void set_bounds(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
    /**
     * Retrieve Descriptibles in box specified by topleft and bottomright when current zoom level is zoom
     * @param topleft
     * @param bottomright
     * @param zoom
     * @return displayed Descriptibles in box
     */
    virtual std::vector<std::unique_ptr<Descriptible> > get_selected(geo::Point const& topleft, geo::Point const& bottomright, int zoom) = 0;
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
