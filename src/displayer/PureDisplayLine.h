/*
 * PureDisplayLine.h
 *
 *  Created on: May 25, 2012
 *      Author: martin
 */

#ifndef PUREDISPLAYLINE_H_
#define PUREDISPLAYLINE_H_

#include "DisplayElement.h"

namespace display
{

class PureDisplayLine : public DisplayElement
{
public:
    PureDisplayLine(geo::Point  const& p1, geo::Point const& p2, std::shared_ptr<DisplayStyle> style);
    DisplayElementType get_type() const;
    bool operator<(DisplayElement const& other) const;
    bool operator==(DisplayElement const& other) const;
    osm::ContainedElement const& get_element() const;
    std::shared_ptr<DisplayElement> copy(std::shared_ptr<DisplayStyle> st);
    virtual ~PureDisplayLine();
protected:
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
private:
    geo::Point p1, p2;
    osm::NeverContainedElement el;
};

} /* namespace display */
#endif /* PUREDISPLAYLINE_H_ */
