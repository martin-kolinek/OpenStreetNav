#include <boost/program_options.hpp>
#include <iostream>
#include <boost/algorithm/string/regex.hpp>
#include <fstream>
#include "../osmdb/osmdb.h"

int main(int argc, char** argv)
{
    boost::program_options::options_description desc("Utility which creates tables holding information used for route planning. Allowed options");
    desc.add_options()
        ("help,h", "print this help")
        ("database,d", boost::program_options::value<std::string>(), "database to work with")
        ("full-schema,f", boost::program_options::value<std::string>(), "schema containing data")
        ("reduced-schema,r", boost::program_options::value<std::string>(), "schema containing reduced ways (see wayreduction)")
        ("output-schema,o", boost::program_options::value<std::string>(), "output database schema")
        ("initialize,n", "create schema, tables keys and indexes")
        ("important,i", boost::program_options::value<std::string>(), "pipe separated key value pairs to consider e.g. (key1/val1|key2/val2)")
        ("important-file,I", boost::program_options::value<std::string>(), "file listing key value pairs to consider (newline separated)");
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
    boost::program_options::notify(vm);
    if (vm.count("help"))
    {
        std::cout << desc << std::endl;
        return 0;
    }
    
    if(!vm.count("full-schema"))
    {
        std::cout<<"full-schema is required"<<std::endl;
        std::cout<<desc<<std::endl;
        return 1;
    }
    
    if(!vm.count("reduced-schema"))
    {
        std::cout<<"reduced-schema is required"<<std::endl;
        std::cout<<desc<<std::endl;
        return 1;
    }
    
    if(!vm.count("output-schema"))
    {
        std::cout<<"output-schema is required"<<std::endl;
        std::cout<<desc<<std::endl;
        return 1;
    }
    
    std::string full, reduced, output;
    full = vm["full-schema"].as<std::string>();
    reduced = vm["reduced-schema"].as<std::string>();
    output = vm["output-schema"].as<std::string>();
    std::string conn_str;
    if(vm.count("database"))
        conn_str = "dbname"+vm["database"].as<std::string>();
    psql::Database f_db(conn_str);
    f_db.set_schema(full);
    osmdb::OsmDatabase f_odb(f_db);
    psql::Database r_db(conn_str);
    r_db.set_schema(reduced+","+full);
    osmdb::OsmDatabase r_odb(r_db);
    psql::Database o_db(conn_str);
    o_db.set_schema(output+","+reduced+","+full);
    osmdb::OsmDatabase o_odb(o_db);
    
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
    
    osmdb::RoadNetworkCreator cr(f_odb, r_odb, o_odb, mp);
    
    if(vm.count("initialize"))
    {
        std::cout<<"Creating table"<<std::endl;
        cr.create_road_network_table();
    }
    
    std::cout<<"Copying data"<<std::endl;
    cr.copy_road_network_data();

}
