#ifndef EDGEHIGHLIGHTER_H_
#define EDGEHIGHLIGHTER_H_

#include "DisplayProvider.h"
#include <vector>
#include "LineDisplayStyle.h"
#include "DisplayStyleChanger.h"

namespace display
{

class EdgeHighlighter : public DisplayProvider
{
public:
    EdgeHighlighter(DisplayProvider& src, std::unique_ptr<DisplayStyleChanger> && style);
    element_range get_display_elements();
    void add_descriptible(Descriptible const& desc);
    void add_way_region(osm::WayRegion const& reg);
    void clear();
    void set_bounds(geo::Point const& , geo::Point const& , int );
    std::vector<std::unique_ptr<Descriptible> > get_selected(geo::Point const& , geo::Point const& , int );
    double center_lat();
    double center_lon();
    int get_min_zoom();
    int get_max_zoom();
    virtual ~EdgeHighlighter();
private:
    DisplayProvider& src;
    std::vector<std::shared_ptr<DisplayElement> > cache;
    std::map<osm::Way, osm::WayRegion, osm::LtByID> highlight;
    std::unique_ptr<DisplayStyleChanger> style;
    bool renew_cache;
};

}

#endif
