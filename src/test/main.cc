/*
 * main.cc
 *
 *  Created on: Feb 4, 2012
 *      Author: martin
 */

#include "../osmdb/osmdb.h"
#include "../elements/osmelements.h"
#include "../psql/psql.h"
#include <ctime>

int main(int, char**)
{
	psql::Database pdb("");
	pdb.set_schema("svk_osm");
	osmdb::OsmDatabase db(pdb);
	osmdb::PropertiesSelection ps(db);
	psql::Statement<psql::BindTypes<>, psql::RetTypes<int64_t> > st("SELECT ID FROM Ways", pdb);
	auto vect = psql::exec_statement_col(st);
	int num = 0;
	std::cout<<"start"<<std::endl;
	auto t = std::time(NULL);
	for(unsigned int i = 0; i<vect.size(); ++i, ++num)
	{
		//std::cout<<clock()-c<<std::endl;
		if(num==10000)
		{
			std::cout<<num/(std::time(NULL)-t)<<" per second"<<std::endl;
			t=std::time(NULL);
			num=0;
		}

		osm::Way w(vect[i]);
		w.fill(ps);
	}
	return 0;
}


