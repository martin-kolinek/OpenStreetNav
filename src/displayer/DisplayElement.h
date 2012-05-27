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
#include "../elements/osmelements.h"

namespace display
{

enum class DisplayElementType
{
    Line = 0,
    PureLine = 1
};

class DisplayElement
{
public:
    DisplayElement(std::shared_ptr<DisplayStyle> style);
    virtual ~DisplayElement();
    virtual void draw(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const;
    virtual DisplayElementType get_type() const = 0;
    virtual bool operator<(DisplayElement const& other) const = 0;
    virtual bool operator>(DisplayElement const& other) const;
    virtual bool operator<=(DisplayElement const& other) const;
    virtual bool operator>=(DisplayElement const& other) const;
    virtual bool operator==(DisplayElement const& other) const = 0;
    virtual bool operator!=(DisplayElement const& other) const;
    virtual osm::ContainedElement const& get_element() const = 0;
    virtual DisplayStyle const& get_style() const;
    virtual std::shared_ptr<DisplayElement> copy(std::shared_ptr<DisplayStyle> st) = 0;
protected:
    std::shared_ptr<DisplayStyle> style;
    virtual void draw_internal(Cairo::RefPtr<Cairo::Context> cr, proj::MapProjection& pr) const = 0;

};

} /* namespace display */
#endif /* DISPLAYELEMENT_H_ */
