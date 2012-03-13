#ifndef IROADNETWORKNODE_H_
#define IROADNETWORKNODE_H_

#include <vector>

namespace roads
{

class IRoadNetworkNode
{
public:
    virtual ~IRoadNetworkNode();
    virtual std::vector<std::pair<double, IRoadNetworkNode const*> > const& get_neighbours() const = 0;
};

}

#endif
