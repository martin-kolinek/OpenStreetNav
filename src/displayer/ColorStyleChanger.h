/*
 * ColorStyleChanger.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef COLORSTYLECHANGER_H_
#define COLORSTYLECHANGER_H_

#include "DisplayStyleChanger.h"

namespace display
{

class ColorStyleChanger : public DisplayStyleChanger
{
public:
    ColorStyleChanger(double red, double green, double blue, double alpha, double thickness_add = 0);
    std::shared_ptr<DisplayStyle> visit(LineDisplayStyle const& s) const;
    virtual ~ColorStyleChanger();
private:
    double red, green, blue, alpha, thickness_add;
};

} /* namespace display */
#endif /* COLORSTYLECHANGER_H_ */
