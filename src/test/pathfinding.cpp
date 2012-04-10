/*
 * pathfinding.cpp
 *
 *  Created on: Jan 27, 2012
 *      Author: martin
 */

#include <boost/test/unit_test.hpp>
#include "../pathfinding/pathfinding.h"
#include "../osmdb/osmdb.h"
#include <map>
#include "../util/range.h"

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
        int operator()(int) const
        {
            return 0;
        }
    };
    class IsEnd
    {
    public:
        bool operator()(int i) const
        {
            return i == 7;
        }
    };
    auto path = pathfind::find_path(1, 0, GetNeighbours(graph), Heuristic(), IsEnd());
    std::vector<int> correct
    {
        1, 3, 6, 7
    };
    BOOST_CHECK(path == correct);
}

BOOST_AUTO_TEST_CASE(astar_test2)
{
    std::map<int, std::vector<std::pair<int, int> > > graph
    {
        std::make_pair(1, std::vector<std::pair<int, int> >{
            std::make_pair(1, 2)
        }),
        std::make_pair(2, std::vector<std::pair<int, int> >{
            std::make_pair(1, 3),
            std::make_pair(1, 4)
        }),
        std::make_pair(3, std::vector<std::pair<int, int> >{
            std::make_pair(1, 4)
        }),
        std::make_pair(4, std::vector<std::pair<int, int> >{
            std::make_pair(2, 5)
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

    std::map<int, int> heur
    {
        std::make_pair(1, 3),
        std::make_pair(2, 2),
        std::make_pair(3, 1),
        std::make_pair(4, 2),
        std::make_pair(5, 0),
    };

    class Heuristic
    {
        std::map<int, int> heur;
    public:
        Heuristic(std::map<int, int> const& heur):
            heur(heur)
        {
        }
        int operator()(int i) const
        {
            return heur.find(i)->second;
        }
    };
    class IsEnd
    {
    public:
        bool operator()(int i) const
        {
            return i == 5;
        }
    };
    auto path = pathfind::find_path(1, 0, GetNeighbours(graph), Heuristic(heur), IsEnd());
    std::vector<int> correct
    {
        1, 2, 4, 5
    };
    BOOST_CHECK(path == correct);
}

BOOST_AUTO_TEST_SUITE_END()

class dbfix
{
public:
    dbfix():
        pdb("")
    {

        psql::execute_sql(pdb, "CREATE SCHEMA testing");
        psql::execute_sql(pdb, "SET search_path TO testing, public");
    }
    ~dbfix()
    {
        if (pdb.in_transaction() || pdb.in_failed_transaction())
            pdb.rollback_transaction();
        psql::execute_sql(pdb, "DROP SCHEMA testing CASCADE");
    }
    psql::Database pdb;
};

BOOST_FIXTURE_TEST_SUITE(pathfind_db, dbfix)

BOOST_AUTO_TEST_CASE(pathfinder)
{
    psql::Database dst("");
    dst.set_schema("testing");
    osmdb::OsmDatabase db(pdb);
    osmdb::OsmDatabase dest(dst);
    db.create_tables();
    db.create_indexes_and_keys();
    osmdb::ElementInsertion ins(db);
    ins.insert_node(osm::Node(1, 0, 0));
    ins.insert_node(osm::Node(2, 0, 1));
    ins.insert_node(osm::Node(3, 1, 0));
    ins.insert_node(osm::Node(4, 1, 1));
    ins.insert_node(osm::Node(5, 2, 0));
    osm::Way w(1);
    w.tags.insert(osm::Tag("k", "v"));
    w.add_node(2);
    w.add_node(1);
    w.add_node(3);
    ins.insert_way(w);
    w.nodes.clear();
    w.id = 2;
    w.add_node(2);
    w.add_node(3);
    w.add_node(5);
    ins.insert_way(w);
    w.nodes.clear();
    w.id = 3;
    w.add_node(4);
    w.add_node(3);
    ins.insert_way(w);
    osmdb::RoadNetworkCreator rnc(db, db, dest, std::multimap<std::string, std::string> {std::make_pair("k", "v")});
    rnc.create_road_network_table();
    rnc.copy_road_network_data();
    osmdb::RoadLister rl(db);
    pathfind::PathFinder pf(rl);

    auto r = pf.find_way(5, 2);
    bool node = util::any(r.get_highlighted() | util::selected([](std::shared_ptr<osm::HashElementContainer> const & w)
    {
        return w->intersects(osm::Edge(osm::Node(0), 0, osm::Node(2), 2, osm::Way(2)));
    }));
    BOOST_CHECK(node);
    BOOST_CHECK(r.get_description().size() > 0);
}

BOOST_AUTO_TEST_SUITE_END()
