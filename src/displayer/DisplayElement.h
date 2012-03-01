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

enum class DisplayElementType
{
    Line = 0
};

class DisplayElement
{
public:
    DisplayElement();
    virtual ~DisplayElement();
    virtual void draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
    virtual DisplayElementType get_type() const = 0;
    virtual bool operator<(DisplayElement const& other) const = 0;
    virtual bool operator>(DisplayElement const& other) const = 0;
    virtual bool operator<=(DisplayElement const& other) const = 0;
    virtual bool operator>=(DisplayElement const& other) const = 0;
    virtual bool operator==(DisplayElement const& other) const = 0;
    virtual bool operator!=(DisplayElement const& other) const = 0;
protected:
    virtual DisplayStyle const& get_style() const = 0;
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const = 0;

};

} /* namespace display */
#endif /* DISPLAYELEMENT_H_ */
