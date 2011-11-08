/*
 * main.cc
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#include <boost/program_options.hpp>
#include <iostream>
#include "../osmdb/osmdb.h"
#include "../xmlparse/XmlParser.h"
#include <functional>

void import(std::string const& inp, std::string const& outp, bool with_indexes)
{
	osmdb::OsmDatabase db(outp);
	osmdb::ElementInsertion ins(db);
	osmxml::XmlParser pars;
	pars.node_signal().connect([&ins](osm::Node const& n){ins.insert_node(n);});
	pars.way_signal().connect([&ins](osm::Way const& w){ins.insert_way(w);});
	pars.relation_signal().connect([&ins](osm::Relation const& r){ins.insert_relation(r);});
	pars.warn_signal().connect([](std::string const& str){std::cout<<"XML warning "<<str<<std::endl;});
	std::cout<<"Starting import"<<std::endl;
	pars.parse_file(inp);
	std::cout<<"Done importing"<<std::endl;
	if(with_indexes)
	{
		std::cout<<"Creating indexes"<<std::endl;
		db.create_indexes();
	}
	std::cout<<"Success"<<std::endl;
}

int main(int argc, char** argv)
{
	boost::program_options::options_description desc("Allowed options");
	desc.add_options()
			("help,h", "print this help")
			("input,i", "input xml file")
			("output,o", "output sqlite file")
			("without-indexes", "disable index creation");
	boost::program_options::variables_map vm;
	boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
	boost::program_options::notify(vm);
	if(vm.count("help"))
	{
		std::cout<<desc<<std::endl;
		return 0;
	}
	if(!vm.count("input"))
	{
		std::cout<<"input file needed"<<std::endl;
		std::cout<<desc<<std::endl;
		return 1;
	}
	if(!vm.count("output"))
	{
		std::cout<<"output file needed"<<std::endl;
		std::cout<<desc<<std::endl;
		return 1;
	}
	bool indexes = !vm.count("without-indexes");
	auto inp = vm["input"].as<std::string>();
	auto out = vm["output"].as<std::string>();
	import(inp, out, indexes);
	return 0;
}
