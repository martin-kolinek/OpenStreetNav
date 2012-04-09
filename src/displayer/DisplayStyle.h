/*
 * DisplayStyle.h
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#ifndef DISPLAYSTYLE_H_
#define DISPLAYSTYLE_H_

#include <cairomm/context.h>
#include <memory>

namespace display
{

class DisplayStyleChanger;

class DisplayStyle
{
public:
    virtual ~DisplayStyle();
    virtual void prepare(Cairo::RefPtr<Cairo::Context> cr) const = 0;
    virtual void exec(Cairo::RefPtr<Cairo::Context> cr) const = 0;
    virtual bool draw_arrow() const;
    virtual std::shared_ptr<DisplayStyle> copy() const = 0;
    virtual std::shared_ptr<DisplayStyle> accept(DisplayStyleChanger const& c) const = 0;
};

} /* namespace display */
#endif /* DISPLAYSTYLE_H_ */
