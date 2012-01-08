/*
 * ImportTableProcessor.h
 *
 *  Created on: Jan 7, 2012
 *      Author: martin
 */

#ifndef IMPORTTABLEPROCESSOR_H_
#define IMPORTTABLEPROCESSOR_H_

#include "OsmDatabase.h"
#include <boost/signal.hpp>

namespace osmdb
{

enum class ImportTableAction
{
	CREATE_IMPORT_PKEY,
	CREATE_IMPORT_INDEX,
	ANALYZE,
	DELETE_NODE_TO_UPDATE,
	DELETE_WAY_TO_UPDATE,
	DELETE_RELATION_TO_UPDATE,
	DELETE_NODE_TO_DELETE,
	DELETE_WAY_TO_DELETE,
	DELETE_RELATION_TO_DELETE,
	DELETE_ORPHAN,
	DELETE_DUPLICIT_NODE,
	DELETE_DUPLICIT_WAY,
	DELETE_DUPLICIT_RELATION,
	DELETE_INCOMPLETE_WAY,
	DELETE_INCOMPLETE_RELATION,
	DELETE_IMPORT_ORPHANS,
	IMPORT_NODE,
	IMPORT_WAY,
	IMPORT_RELATION,
	IMPORT_NODE_ATTR,
	IMPORT_WAY_ATTR,
	IMPORT_REL_ATTR,
	IMPORT_WAY_NODE,
	IMPORT_MEMBER_NODE,
	IMPORT_MEMBER_WAY,
	IMPORT_MEMBER_REL,
	IMPORT_EDGES,
	CLEAR_IMPORT
};

class ImportTableProcessor
{
public:
	ImportTableProcessor(OsmDatabase& db);
	boost::signal<void (ImportTableAction, int64_t)> action_signal;
	boost::signal<bool (), util::All> proceed_signal;
	void process();
	virtual ~ImportTableProcessor();
private:
	OsmDatabase& db;
};

} /* namespace osmdb */
#endif /* IMPORTTABLEPROCESSOR_H_ */
