/*
 * Thickener.h
 *
 *  Created on: Apr 11, 2012
 *      Author: martin
 */

#ifndef THICKENER_H_
#define THICKENER_H_

#include "DisplayStyleChanger.h"

namespace display
{

class Thickener : public DisplayStyleChanger
{
public:
    Thickener(double addition);
    std::shared_ptr<DisplayStyle> visit(LineDisplayStyle const& s) const;
    virtual ~Thickener();
private:
    double addition;
};

} /* namespace display */
#endif /* THICKENER_H_ */
