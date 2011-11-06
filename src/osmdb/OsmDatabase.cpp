/*
 * OsmDatabase.cpp
 *
 *  Created on: Nov 5, 2011
 *      Author: martin
 */

#include "OsmDatabase.h"

namespace osmdb
{

OsmDatabase::OsmDatabase(std::string const& file):
		nodes_table("Nodes"),
		ways_table("Ways"),
		edges_table("Edges"),
		relations_table("Relations"),
		relation_contents_table("RelationContents"),
		attributes_table("Attributes"),
		db(file)
{
}

OsmDatabase::~OsmDatabase()
{
}

}
