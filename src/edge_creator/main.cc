/*
 * main.cc
 *
 *  Created on: Mar 18, 2012
 *      Author: martin
 */



#include <boost/program_options.hpp>
#include <iostream>
#include <boost/algorithm/string/regex.hpp>
#include <fstream>
#include <boost/property_tree/xml_parser.hpp>
#include "../osmdb/osmdb.h"

int main(int argc, char** argv)
{
    boost::program_options::options_description desc("Creates edges table used . Allowed options");
    desc.add_options()
    ("help,h", "print this help")
    ("database,d", boost::program_options::value<std::string>(), "database to work with")
    ("xml-file,x", boost::program_options::value<std::string>(), "edge description xml")
    ("input-schema,i", boost::program_options::value<std::string>(), "schema containing data")
    ("output-schema,o", boost::program_options::value<std::string>(), "output schema")
    ("create-schema,c", "create output schema");
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
    boost::program_options::notify(vm);
    if (vm.count("help"))
    {
        std::cout << desc << std::endl;
        return 0;
    }
    if (!vm.count("xml-file"))
    {
        std::cout << "xml-file required" << std::endl;
        std::cout << desc << std::endl;
    }
    if (!vm.count("input-schema") && !vm.count("output-schema"))
    {
        std::cout << "output schema or input schema required" << std::endl;
        std::cout << desc << std::endl;
    }
    std::string input;
    std::string output;
    if (vm.count("input-schema"))
        input = vm["input-schema"].as<std::string>();
    else
        input = vm["output-schema"].as<std::string>();
    if (vm.count("output-schema"))
        output = vm["output-schema"].as<std::string>();
    else
        output = vm["input-schema"].as<std::string>();
    std::string conn_str;
    if (vm.count("database"))
        conn_str = "dbname" + vm["database"].as<std::string>();
    psql::Database pdb(conn_str);
    if (vm.count("create-schema"))
        pdb.create_schema(output);
    pdb.set_schema(output + "," + input);
    osmdb::OsmDatabase db(pdb);
    boost::property_tree::ptree tr;
    boost::property_tree::xml_parser::read_xml(vm["xml-file"].as<std::string>(), tr, boost::property_tree::xml_parser::trim_whitespace);
    osmdb::EdgeCreator ecr(db, tr);
    std::cout << "Creating tables" << std::endl;
    ecr.create_tables();
    std::cout << "Inserting data" << std::endl;
    ecr.insert_data();
    std::cout << "Creating keys and indexes" << std::endl;
    ecr.create_keys_and_indexes();
    std::cout << "Done" << std::endl;

}
