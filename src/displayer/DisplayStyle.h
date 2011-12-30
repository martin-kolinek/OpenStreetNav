/*
 * DisplayStyle.h
 *
 *  Created on: Dec 7, 2011
 *      Author: martin
 */

#ifndef DISPLAYSTYLE_H_
#define DISPLAYSTYLE_H_

#include <cairomm/context.h>

namespace display
{

class DisplayStyle
{
public:
    virtual ~DisplayStyle();
    virtual void prepare(Cairo::RefPtr<Cairo::Context> cr) const = 0;
    virtual void exec(Cairo::RefPtr<Cairo::Context> cr) const = 0;
};

} /* namespace display */
#endif /* DISPLAYSTYLE_H_ */
