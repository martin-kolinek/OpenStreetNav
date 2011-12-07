/*
 * DisplayElement.h
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#ifndef DISPLAYELEMENT_H_
#define DISPLAYELEMENT_H_

#include "DisplayStyle.h"
#include <memory>
#include <cairomm/context.h>
#include "../projection/projection.h"

namespace display
{

class DisplayElement
{
public:
    DisplayElement(std::unique_ptr<DisplayStyle> disp);
    virtual ~DisplayElement();
    virtual void draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr);
protected:
    std::unique_ptr<DisplayStyle> disp;
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) = 0;

};

} /* namespace display */
#endif /* DISPLAYELEMENT_H_ */
