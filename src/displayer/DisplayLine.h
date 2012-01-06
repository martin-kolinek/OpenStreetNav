/*
 * DisplayLine.h
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#ifndef DISPLAYLINE_H_
#define DISPLAYLINE_H_

#include "DisplayElement.h"

namespace display
{

class DisplayLine : public DisplayElement
{
public:
    DisplayLine(geo::Point p1, geo::Point p2, bool arrow, std::unique_ptr<DisplayStyle> && style);
    DisplayElementType get_type() const;
    bool operator<(DisplayElement const& other) const;
    bool operator>(DisplayElement const& other) const;
    bool operator<=(DisplayElement const& other) const;
    bool operator>=(DisplayElement const& other) const;
    bool operator==(DisplayElement const& other) const;
    bool operator!=(DisplayElement const& other) const;
    virtual ~DisplayLine();
protected:
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
private:
    geo::Point p1, p2;
    bool arrow;
};

} /* namespace display */
#endif /* DISPLAYLINE_H_ */
