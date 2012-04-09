/*
 * DisplayStyleChanger.h
 *
 *  Created on: Apr 9, 2012
 *      Author: martin
 */

#ifndef DISPLAYSTYLECHANGER_H_
#define DISPLAYSTYLECHANGER_H_

#include "LineDisplayStyle.h"

namespace display
{

class DisplayStyleChanger
{
public:
    virtual std::shared_ptr<DisplayStyle> visit(LineDisplayStyle const& s) const;
    virtual std::shared_ptr<DisplayStyle> visit(DisplayStyle const& s) const;
    virtual ~DisplayStyleChanger();
};

} /* namespace display */
#endif /* DISPLAYSTYLECHANGER_H_ */
