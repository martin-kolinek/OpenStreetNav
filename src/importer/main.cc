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
    if (i % 100000 == 0)
        std::cout << "Processed approximately " << i << " elements" << std::endl;
}

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
        case ImportTableAction::DELETE_DUPLICIT_NODE:
            std::cout << "Deleted " << amount << " duplicit nodes in import" << std::endl;
            break;
        case ImportTableAction::DELETE_DUPLICIT_RELATION:
            std::cout << "Deleted " << amount << " duplicit relations in import" << std::endl;
            break;
        case ImportTableAction::DELETE_DUPLICIT_WAY:
            std::cout << "Deleted " << amount << " duplicit ways in import" << std::endl;
            break;
        case ImportTableAction::DELETE_IMPORT_ORPHANS:
            std::cout << "Deleted " << amount << " orphan elements in import" << std::endl;
            break;
        case ImportTableAction::DELETE_INCOMPLETE_RELATION:
            std::cout << "Deleted " << amount << " incomplete relations in import" << std::endl;
            break;
        case ImportTableAction::DELETE_INCOMPLETE_WAY:
            std::cout << "Deleted " << amount << " incomplete ways in import" << std::endl;
            break;
        case ImportTableAction::DELETE_NODE_TO_DELETE:
            std::cout << "Deleted " << amount << " nodes which should be deleted" << std::endl;
            break;
        case ImportTableAction::DELETE_NODE_TO_UPDATE:
            std::cout << "Deleted " << amount << " nodes which should be updated" << std::endl;
            break;
        case ImportTableAction::DELETE_ORPHAN:
            std::cout << "Deleted " << amount << " orphan elements" << std::endl;
            break;
        case ImportTableAction::DELETE_RELATION_TO_DELETE:
            std::cout << "Deleted " << amount << " relations which should be deleted" << std::endl;
            break;
        case ImportTableAction::DELETE_RELATION_TO_UPDATE:
            std::cout << "Deleted " << amount << " relations which should be updated" << std::endl;
            break;
        case ImportTableAction::DELETE_WAY_TO_DELETE:
            std::cout << "Deleted " << amount << " ways which should be deleted" << std::endl;
            break;
        case ImportTableAction::DELETE_WAY_TO_UPDATE:
            std::cout << "Deleted " << amount << " ways which should be updated" << std::endl;
            break;
        case ImportTableAction::IMPORT_EDGES:
            std::cout << "Imported " << amount << " edges" << std::endl;
            break;
        case ImportTableAction::IMPORT_MEMBER_NODE:
            std::cout << "Imported " << amount << " member nodes" << std::endl;
            break;
        case ImportTableAction::IMPORT_MEMBER_REL:
            std::cout << "Imported " << amount << " member relations" << std::endl;
            break;
        case ImportTableAction::IMPORT_MEMBER_WAY:
            std::cout << "Imported " << amount << " member ways" << std::endl;
            break;
        case ImportTableAction::IMPORT_NODE:
            std::cout << "Imported " << amount << " nodes" << std::endl;
            break;
        case ImportTableAction::IMPORT_NODE_ATTR:
            std::cout << "Imported " << amount << " node attributes" << std::endl;
            break;
        case ImportTableAction::IMPORT_RELATION:
            std::cout << "Imported " << amount << " relations" << std::endl;
            break;
        case ImportTableAction::IMPORT_REL_ATTR:
            std::cout << "Imported " << amount << " relation attributes" << std::endl;
            break;
        case ImportTableAction::IMPORT_WAY:
            std::cout << "Imported " << amount << " ways" << std::endl;
            break;
        case ImportTableAction::IMPORT_WAY_ATTR:
            std::cout << "Imported " << amount << " way attributes" << std::endl;
            break;
        case ImportTableAction::IMPORT_WAY_NODE:
            std::cout << "Imported " << amount << " way nodes" << std::endl;
            break;
        case ImportTableAction::DELETE_DUPLICIT_ATTR:
        	std::cout << "Deleted " << amount << " duplicit attributes in import" << std::endl;
        	break;
        case ImportTableAction::DELETE_DUPLICIT_WAYNODE:
        	std::cout << "Deleted " << amount << " duplicit waynodes in import" << std::endl;
        	break;
        case ImportTableAction::DELETE_DUPLICIT_MEMBER:
        	std::cout << "Deleted " << amount << " duplicit member elements in import" << std::endl;
            break;
    }
}

bool proceed_handler()
{
    while (true)
    {
        std::cout << "Proceed? (y/n)" << std::endl;
        std::string s;
        std::cin >> s;
        if (s == "y" || s == "yes")
        {
            return true;
        }
        if (s == "n" || s == "no")
        {
            return false;
        }
        std::cout << "Sorry, didn't understand, try again" << std::endl;
    }
    return true;
}

void copy_to_db(std::string const& filename, osmdb::OsmDatabase& db)
{
	osmdb::ElementCopy ins(db);
	osmxml::XmlParser pars;
	int done = 0;
	pars.node_handler = [&ins](osm::Node const & n)
	{
		ins.insert_node(n);
	};
	pars.way_handler = [&ins](osm::Way const & w)
	{
		ins.insert_way(w);
	};
	pars.relation_handler = [&ins](osm::Relation const & r)
	{
		ins.insert_relation(r);
	};
	pars.progress_handler = [&done]()
	{
		progress(done);
	};
	std::cout << "Starting copy" << std::endl;
	ins.start_copy();
	pars.parse_file(filename);
	ins.end_copy();
	std::cout << "Done copying" << std::endl;
}

bool process_import_table(osmdb::OsmDatabase& db, bool quiet)
{
	osmdb::ImportTableProcessor proc(db);
	bool status=false;
	proc.proceed_signal.connect([&]()
	{
		if(quiet)
			status = true;
		else
			status = proceed_handler();
		if(status)
			db.drop_primary_keys();
		return status;
	});
	proc.action_signal.connect(action_handler);
	proc.process();
	return status;
}

int import(std::string const& inp, std::string const& dbname, std::string const& schema, bool init, bool recreate, bool copy, bool import, bool quiet, bool analyze)
{
	try
	{
		std::string connstr;
		if(dbname!="")
			connstr="dbname=" + dbname;
		psql::Database pdb(connstr, true);
		if(init && schema!="")
			pdb.create_schema(schema);
		if(schema!="")
			pdb.set_schema(schema);
		osmdb::OsmDatabase db(pdb);
		if (init)
		{
			db.create_tables();
			db.create_indexes_and_keys();
		}
		pdb.begin_transaction();
		if (recreate)
		{
			std::cout << "Dropping foreign keys and indexes" << std::endl;
			db.drop_foreign_keys();
			db.drop_indexes();
		}
		std::cout << "Starting import" << std::endl;
		bool status = true;
		if(copy)
			copy_to_db(inp, db);
		if(import)
			status = process_import_table(db, quiet);
		if (recreate)
		{
			std::cout << "Recreating indexes and keys" << std::endl;
			db.create_indexes_and_keys();
		}
		if(status)
		{
			pdb.commit_transaction();
			if(analyze)
			{
				std::cout<<"Running analyze"<<std::endl;
				pdb.analyze();
			}
			std::cout << "Success" << std::endl;
		}
		else
		{
			std::cout <<"Cancelling changes"<<std::endl;
			pdb.rollback_transaction();
		}
	}
	catch(xml_schema::parsing& p)
	{
		std::cout<<"Error parsing input document"<<std::endl<<p;
		return 1;
	}
	catch(std::exception& ex)
	{
		std::cout<<"Error occured"<<std::endl<<ex.what()<<std::endl;
		return 1;
	}
	return 0;
}

int main(int argc, char** argv)
{
    boost::program_options::options_description desc("Allowed options");
    desc.add_options()
    ("help,h", "print this help")
    ("input,i", boost::program_options::value<std::string>(), "input xml file")
    ("output-db,d", boost::program_options::value<std::string>(), "output postgresql database name")
    ("output-schema,s", boost::program_options::value<std::string>(), "output postgresql database schema")
    ("initialize,I", "create schema, tables keys and indexes")
    ("recreate-indexes,r", "drop indexes and keys before import and recreate afterward")
    ("no-questions,q", "do not ask questions")
    ("do-import,e", "execute import procedure (put data into appropriate tables)")
    ("analyze,a", "run analyze on database");
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
    boost::program_options::notify(vm);
    if (vm.count("help"))
    {
        std::cout << desc << std::endl;
        return 0;
    }
    bool proc_inp=false;
    std::string inp;
    if (vm.count("input"))
    {
        inp=vm["input"].as<std::string>();
        proc_inp=true;
    }
    std::string db;
    if (vm.count("output-db"))
    {
        db=vm["output-db"].as<std::string>();
    }
    std::string sch;
    if (vm.count("output-schema"))
    	sch = vm["output-schema"].as<std::string>();
    bool init = vm.count("initialize");
    bool recreate = vm.count("recreate-indexes");
    bool quiet = vm.count("no-questions");
    bool process = vm.count("do-import");
    bool analyze = vm.count("analyze");
    if(!init && !analyze && !process && !proc_inp)
    {
    	std::cout<<"No action to be done"<<std::endl;
    	std::cout<<desc<<std::endl;
    	return 0;
    }
    return import(inp, db, sch, init, recreate, proc_inp, process, quiet, analyze);

}
