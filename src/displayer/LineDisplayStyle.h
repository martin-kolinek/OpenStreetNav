/*
 * LineDisplayStyle.h
 *
 *  Created on: Dec 29, 2011
 *      Author: martin
 */

#ifndef LINEDISPLAYSTYLE_H_
#define LINEDISPLAYSTYLE_H_

#include "DisplayStyle.h"

namespace display
{


class LineDisplayStyle : public DisplayStyle
{
public:
    LineDisplayStyle(double red, double green, double blue, double alpha, double thickness, ArrowStyle arrow);
    virtual ~LineDisplayStyle();
    void prepare(Cairo::RefPtr<Cairo::Context> cr) const;
    void exec(Cairo::RefPtr<Cairo::Context> cr) const;
    ArrowStyle draw_arrow() const;
    std::shared_ptr<DisplayStyle> copy() const;
    std::shared_ptr<DisplayStyle> accept(DisplayStyleChanger const& c) const;
    double red, green, blue, alpha;
    ArrowStyle arrow;
    double get_thickness() const;
private:
    double thickness;
};

} /* namespace display */
#endif /* LINEDISPLAYSTYLE_H_ */
