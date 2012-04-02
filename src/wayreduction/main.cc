/*
 * main.cc
 *
 *  Created on: Jan 15, 2012
 *      Author: martin
 */

#include <string>
#include "../osmdb/osmdb.h"
#include "wayreduction.h"
#include <boost/program_options.hpp>
#include <boost/algorithm/string/regex.hpp>
#include <fstream>
#include "WayNodeFilter.h"

void action_handler(osmdb::ImportTableAction act, int64_t amount)
{
    using osmdb::ImportTableAction;
    switch (act)
    {
        case ImportTableAction::ANALYZE:
            std::cout << "Done analyzing" << std::endl;
            break;
        case ImportTableAction::CLEAR_IMPORT:
            std::cout << "Done cleaning up" << std::endl;
            break;
        case ImportTableAction::CREATE_IMPORT_INDEX:
            std::cout << "Created import indexes" << std::endl;
            break;
        case ImportTableAction::CREATE_IMPORT_PKEY:
            std::cout << "Created import primary key" << std::endl;
            break;
        case ImportTableAction::IMPORT_EDGES:
            std::cout << "Imported " << amount << " edges" << std::endl;
            break;
        case ImportTableAction::IMPORT_WAY_NODE:
            std::cout << "Imported " << amount << " way nodes" << std::endl;
            break;
        default:
            break;
    }
}

int reduce(std::string const& dbname, std::string const& base, std::string dest, std::multimap<std::string, std::string> mp, double d)
{
    try
    {
        std::string connstr;
        if (dbname != "")
            connstr = "dbname=" + dbname;
        psql::Database pdb(connstr, true);
        psql::Database pdb2(connstr, true);
        pdb.set_schema(base);
        pdb2.create_schema(dest);
        if (base != "")
            dest += "," + base;
        pdb2.set_schema(dest);
        osmdb::OsmDatabase odb(pdb);
        osmdb::OsmDatabase odb2(pdb2);
        odb2.create_edge_tables();
        osmdb::ElementCopy cp(odb2);

        wayred::WayNodeFilter flt(d);
        for (auto it = mp.begin(); it != mp.end(); ++it)
            flt.add_important(it->first, it->second);
        cp.start_copy();
        pdb.begin_transaction();
        osmdb::WayLister wl(odb, mp);
        std::cout << "Processing ways";
        auto reduced_ways = flt.process_range(wl.get_range());
        for (auto it = reduced_ways.begin(); it != reduced_ways.end(); ++it)
        {
            cp.insert_way(*it);
        }
        std::cout << std::endl;
        pdb.commit_transaction();
        cp.end_copy();
        pdb2.begin_transaction();
        osmdb::ImportTableProcessor proc(odb2);
        proc.disable_all();
        proc.enable(osmdb::ImportTableAction::CLEAR_IMPORT);
        proc.enable(osmdb::ImportTableAction::ANALYZE);
        proc.enable(osmdb::ImportTableAction::CREATE_IMPORT_INDEX);
        proc.enable(osmdb::ImportTableAction::CREATE_IMPORT_PKEY);
        proc.enable(osmdb::ImportTableAction::IMPORT_EDGES);
        proc.enable(osmdb::ImportTableAction::IMPORT_WAY_NODE);
        proc.action_signal.connect(action_handler);
        proc.process();
        odb2.create_edge_keys_and_indexes();
        pdb2.commit_transaction();
        std::cout << "Success" << std::endl;
    }
    catch (std::exception& ex)
    {
        std::cout << "Error occured" << std::endl << ex.what() << std::endl;
        return 1;
    }
    return 0;
}

int main (int argc, char** argv)
{
    boost::program_options::options_description desc("Allowed options");
    desc.add_options()
    ("help,h", "print this help")
    ("database,d", boost::program_options::value<std::string>(), "postgresql database to operate on")
    ("base-schema,b", boost::program_options::value<std::string>(), "base schema name - this needs to exist")
    ("dest-schema,s", boost::program_options::value<std::string>(), "destination schema name - this shouldn't exist")
    ("important,i", boost::program_options::value<std::string>(), "pipe separated key value pairs to consider e.g. (key1/val1|key2/val2)")
    ("distance-limit,l", boost::program_options::value<double>(), "node will not be filtered if it is this far from last non filtered node")
    ("important-file,I", boost::program_options::value<std::string>(), "file listing key value pairs to consider (newline separated)");
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
    boost::program_options::notify(vm);
    if (vm.count("help"))
    {
        std::cout << desc << std::endl;
        return 0;
    }
    std::string dbname;
    if (vm.count("database"))
        dbname = vm["database"].as<std::string>();
    std::string base;
    if (vm.count("base-schema"))
        base = vm["base-schema"].as<std::string>();
    if (!vm.count("dest-schema"))
    {
        std::cout << "You need to specify destination schema" << std::endl << desc << std::endl;
    }
    double d = std::numeric_limits<double>::infinity();
    if (vm.count("distance-limit"))
        d = vm["distance-limit"].as<double>();
    std::string dest(vm["dest-schema"].as<std::string>());
    std::multimap<std::string, std::string> mp;
    boost::regex r("\\|");
    boost::regex r2("\\/");
    if (vm.count("important"))
    {
        std::vector<std::string> vect;
        boost::algorithm::split_regex(vect, vm["important"].as<std::string>(), r);
        for (unsigned int i = 0; i < vect.size(); ++i)
        {
            std::vector<std::string> v2;
            boost::algorithm::split_regex(v2, vect[i], r2);
            if (v2.size() != 2)
            {
                std::cout << "Error parsing important attributes" << std::endl;
                return 1;
            }
            mp.insert(std::make_pair(v2[0], v2[1]));
        }
    }
    if (vm.count("important-file"))
    {
        std::ifstream ifs(vm["important-file"].as<std::string>(), std::ifstream::in);
        std::string s;
        while (!ifs.eof())
        {
            ifs >> s;
            std::vector<std::string> v2;
            boost::algorithm::split_regex(v2, s, r2);
            if (v2.size() != 2)
            {
                std::cout << "Error parsing important attributes" << std::endl;
                return 1;
            }
            mp.insert(std::make_pair(v2[0], v2[1]));
        }
    }
    return reduce(dbname, base, dest, mp, d);
}
