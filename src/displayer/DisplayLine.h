/*
 * DisplayLine.h
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#ifndef DISPLAYLINE_H_
#define DISPLAYLINE_H_

#include "DisplayElement.h"
#include "LineDisplayStyle.h"
#include "../elements/osmelements.h"

namespace display
{

class DisplayLine : public DisplayElement
{
public:
    DisplayLine(osm::Edge const& edge, std::shared_ptr<DisplayStyle> style);
    DisplayElementType get_type() const;
    bool operator<(DisplayElement const& other) const;
    bool operator==(DisplayElement const& other) const;
    osm::ContainedElement const& get_element() const;
    std::shared_ptr<DisplayElement> copy(std::shared_ptr<DisplayStyle> st);
    virtual ~DisplayLine();
    virtual DisplayStyle const& get_style() const;
protected:
    std::shared_ptr<DisplayStyle> style;
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
private:
    osm::Edge const& edge;
};

} /* namespace display */
#endif /* DISPLAYLINE_H_ */
