#ifndef EDGEHIGHLIGHTER_H_
#define EDGEHIGHLIGHTER_H_

#include "DisplayProvider.h"
#include <vector>
#include "LineDisplayStyle.h"
#include "EdgeHighlightable.h"

namespace display
{

class EdgeHighlighter : public DisplayProvider
{
public:
    EdgeHighlighter(EdgeHighlightable& src, LineDisplayStyle const& style);
    element_range get_display_elements();
    void add_descriptible(Descriptible const& desc);
    void clear();
    void set_bounds(geo::Point const& , geo::Point const& , int );
    std::vector<std::unique_ptr<Descriptible> > get_selected(geo::Point const& , geo::Point const& , int );
    double center_lat();
    double center_lon();
    virtual ~EdgeHighlighter();
private:
    EdgeHighlightable& src;
    std::vector<std::shared_ptr<DisplayElement> > cache;
    std::map<osm::Way, osm::WayRegion, osm::LtByID> highlight;
    LineDisplayStyle style;
    bool renew_cache;
};

}

#endif
