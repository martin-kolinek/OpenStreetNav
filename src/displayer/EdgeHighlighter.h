#ifndef EDGEHIGHLIGHTER_H_
#define EDGEHIGHLIGHTER_H_

#include "DisplayProvider.h"
#include <vector>
#include "LineDisplayStyle.h"
#include "DisplayStyleChanger.h"
#include "Descriptible.h"
#include "../elements/osmelements.h"

namespace display
{

class EdgeHighlighter : public DisplayProvider
{
public:
    EdgeHighlighter(DisplayProvider& src, std::unique_ptr<DisplayStyleChanger> && style);
    element_range get_display_elements();
    void add_descriptible(Descriptible const& desc);
    void clear();
    void set_bounds(geo::Point const& , geo::Point const& , int );
    std::vector<std::shared_ptr<Descriptible> > get_selected(geo::Point const& , geo::Point const& , int );
    double center_lat();
    double center_lon();
    int get_min_zoom();
    int get_max_zoom();
    virtual ~EdgeHighlighter();
private:
    DisplayProvider& src;
    std::vector<std::shared_ptr<DisplayElement> > cache;
    osm::CompositeElementContainer highlight;
    std::unique_ptr<DisplayStyleChanger> style;
    bool renew_cache;
};

}

#endif
