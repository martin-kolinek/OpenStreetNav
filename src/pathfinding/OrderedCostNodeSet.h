/*
 * OrderedCostNodeSet.h
 *
 *  Created on: Jan 27, 2012
 *      Author: martin
 */

#ifndef ORDEREDCOSTNODESET_H_
#define ORDEREDCOSTNODESET_H_

#include <set>
#include <unordered_map>
#include "../util.h"

namespace pathfind
{

template<typename Node, typename Cost, typename CostCompare>
class OrderedCostNodeSet
{
private:
    std::multiset<std::pair<Cost, Node>, util::CompareFirst<Cost, Node, CostCompare> > st;
    std::unordered_map<Node, Cost> mp;
public:
    OrderedCostNodeSet()
    {
    }
    void insert(Cost const& c, Node const& n)
    {
        auto it = mp.find(n);
        if (it != mp.end())
            st.erase(std::make_pair(it->second, it->first));
        st.insert(std::make_pair(c, n));
        mp[n] = c;
    }
    typename std::multiset<std::pair<Cost, Node> >::const_iterator find(Node const& n) const
    {
        auto it = mp.find(n);
        if (it == mp.end())
            return st.end();
        return st.find(std::make_pair(it->second, it->first));
    }
    typename std::multiset<std::pair<Cost, Node> >::const_iterator end() const
    {
        return st.end();
    }
    bool empty()
    {
        return st.empty();
    }
    Node top()
    {
        return st.begin()->second;
    }
    void pop()
    {
        st.erase(st.begin());
    }
};

} /* namespace pathfind */
#endif /* ORDEREDCOSTNODESET_H_ */
