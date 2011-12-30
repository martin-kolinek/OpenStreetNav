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
    DisplayLine(geo::Point p1, geo::Point p2, std::unique_ptr<DisplayStyle> && style);
    virtual ~DisplayLine();
protected:
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
private:
    geo::Point p1, p2;
};

} /* namespace display */
#endif /* DISPLAYLINE_H_ */
