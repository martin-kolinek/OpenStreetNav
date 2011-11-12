/*
 * main.cc
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#include <boost/program_options.hpp>
#include <iostream>
#include "../osmdb/osmdb.h"
#include "../xmlparse/xmlparse.h"
#include <functional>

void progress(int& i)
{
	i++;
	if(i%100000==0)
		std::cout<<"Processed approximately "<<i<<" xml elements"<<std::endl;
}

void import(std::string const& inp, std::string const& outp, bool with_indexes)
{
	osmdb::OsmDatabase db(outp);
	sqlite::execute_sql("BEGIN TRANSACTION", db.get_db());
	osmdb::ElementInsertion ins(db);
	osmxml::XmlParser pars;
	int done=0;
	pars.node_handler=[&ins](osm::Node const& n){ins.insert_node(n);};
	pars.way_handler=[&ins](osm::Way const& w){ins.insert_way(w);};
	pars.relation_handler=[&ins](osm::Relation const& r){ins.insert_relation(r);};
	pars.warn_handler=[](std::string const& str){std::cout<<"XML warning "<<str<<std::endl;};
	pars.progress_handler=[&done](){progress(done);};
	std::cout<<"Starting import"<<std::endl;
	pars.parse_file(inp);
	std::cout<<"Done importing"<<std::endl;
	sqlite::execute_sql("COMMIT TRANSACTION", db.get_db());
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
			("input,i", boost::program_options::value<std::string>(), "input xml file")
			("output,o", boost::program_options::value<std::string>(), "output sqlite file")
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
	std::string inp = vm["input"].as<std::string>();
	std::string out = vm["output"].as<std::string>();
	import(inp, out, indexes);
	return 0;
}
