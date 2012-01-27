/*
 * pathfinding.cpp
 *
 *  Created on: Jan 27, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../pathfinding/pathfinding.h"
#include <map>

BOOST_AUTO_TEST_SUITE(pathfinding)

BOOST_AUTO_TEST_CASE(astar_test)
{
    std::map<int, std::vector<std::pair<int, int> > > graph
    {
        std::make_pair(1, std::vector<std::pair<int, int> >{
            std::make_pair(2, 2),
            std::make_pair(1, 3),
            std::make_pair(3, 4)
        }),
        std::make_pair(2, std::vector<std::pair<int, int> >{
            std::make_pair(2, 1),
            std::make_pair(3, 3),
            std::make_pair(4, 7)
        }),
        std::make_pair(3, std::vector<std::pair<int, int> >{
            std::make_pair(1, 1),
            std::make_pair(3, 2),
            std::make_pair(1, 6)
        }),
        std::make_pair(4, std::vector<std::pair<int, int> >{
            std::make_pair(3, 1),
            std::make_pair(2, 5)
        }),
        std::make_pair(5, std::vector<std::pair<int, int> >{
            std::make_pair(3, 6),
            std::make_pair(2, 4)
        }),
        std::make_pair(6, std::vector<std::pair<int, int> >{
            std::make_pair(3, 5),
            std::make_pair(1, 3),
            std::make_pair(1, 7)
        }),
        std::make_pair(7, std::vector<std::pair<int, int> >{
            std::make_pair(4, 2),
            std::make_pair(1, 6)
        })
    };
    class GetNeighbours
    {
    private:
        std::map<int, std::vector<std::pair<int, int> > >& mp;
    public:
        GetNeighbours(std::map<int, std::vector<std::pair<int, int> > >& mp):
            mp(mp)
        {
        }
        std::vector<std::pair<int, int> > operator()(int nd) const
        {
            return mp[nd];
        }
    };
    class Heuristic
    {
    public:
        int operator()(int, int) const
        {
            return 0;
        }
    };
    auto path = pathfind::find_path(1, 0, 7, GetNeighbours(graph), Heuristic());
    std::vector<int> correct
    {
        1, 3, 6, 7
    };
    BOOST_CHECK(path == correct);
}

BOOST_AUTO_TEST_SUITE_END()


