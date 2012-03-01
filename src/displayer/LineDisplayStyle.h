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
    LineDisplayStyle(double red, double green, double blue, double alpha, double thickness, bool arrow);
    virtual ~LineDisplayStyle();
    void prepare(Cairo::RefPtr<Cairo::Context> cr) const;
    void exec(Cairo::RefPtr<Cairo::Context> cr) const;
    bool draw_arrow() const;
private:
    double red, green, blue, alpha, thickness;
    bool arrow;
};

} /* namespace display */
#endif /* LINEDISPLAYSTYLE_H_ */
