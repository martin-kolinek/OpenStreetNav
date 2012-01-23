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

/**
 * Actions which ImportTableProcessor can do
 */
enum class ImportTableAction
{
    CREATE_IMPORT_PKEY,        //!< CREATE_IMPORT_PKEY create primary key on Import table
    CREATE_IMPORT_INDEX,       //!< CREATE_IMPORT_INDEX create indexes on Import table
    ANALYZE,                   //!< ANALYZE analyze the import table
    DELETE_NODE_TO_UPDATE,     //!< DELETE_NODE_TO_UPDATE delete nodes which would be replaced by import
    DELETE_WAY_TO_UPDATE,      //!< DELETE_WAY_TO_UPDATE delete ways which would be replaced by import
    DELETE_RELATION_TO_UPDATE, //!< DELETE_RELATION_TO_UPDATE delete relations which would be replaced by import
    DELETE_NODE_TO_DELETE,     //!< DELETE_NODE_TO_DELETE delete nodes marked for deletion
    DELETE_WAY_TO_DELETE,      //!< DELETE_WAY_TO_DELETE delete ways marked for deletion
    DELETE_RELATION_TO_DELETE, //!< DELETE_RELATION_TO_DELETE delete relations marked for deletion
    DELETE_ORPHAN,             //!< DELETE_ORPHAN delete orphan elements in tables
    DELETE_DUPLICIT_NODE,      //!< DELETE_DUPLICIT_NODE delete duplicit nodes in import
    DELETE_DUPLICIT_WAY,       //!< DELETE_DUPLICIT_WAY delete duplicit ways in import
    DELETE_DUPLICIT_RELATION,  //!< DELETE_DUPLICIT_RELATION delete duplicit relations in import
    DELETE_INCOMPLETE_WAY,     //!< DELETE_INCOMPLETE_WAY delete incomplete ways in import
    DELETE_INCOMPLETE_RELATION,//!< DELETE_INCOMPLETE_RELATION delete incomplete relations in import
    DELETE_IMPORT_ORPHANS,     //!< DELETE_IMPORT_ORPHANS delete orphan subelements in import
    IMPORT_NODE,               //!< IMPORT_NODE
    IMPORT_WAY,                //!< IMPORT_WAY
    IMPORT_RELATION,           //!< IMPORT_RELATION
    IMPORT_NODE_ATTR,          //!< IMPORT_NODE_ATTR
    IMPORT_WAY_ATTR,           //!< IMPORT_WAY_ATTR
    IMPORT_REL_ATTR,           //!< IMPORT_REL_ATTR
    IMPORT_WAY_NODE,           //!< IMPORT_WAY_NODE
    IMPORT_MEMBER_NODE,        //!< IMPORT_MEMBER_NODE
    IMPORT_MEMBER_WAY,         //!< IMPORT_MEMBER_WAY
    IMPORT_MEMBER_REL,         //!< IMPORT_MEMBER_REL
    IMPORT_EDGES,              //!< IMPORT_EDGES
    CLEAR_IMPORT,              //!< CLEAR_IMPORT clear import table with indexes and primary key
    DELETE_DUPLICIT_ATTR,      //!< DELETE_DUPLICIT_ATTR delete duplicit attributes in import
    DELETE_DUPLICIT_MEMBER,    //!< DELETE_DUPLICIT_MEMBER delete duplicit relation members in import
    DELETE_DUPLICIT_WAYNODE    //!< DELETE_DUPLICIT_WAYNODE delete duplicit nodes in ways in import
};

class AllImportActions
{
public:
    static std::vector<ImportTableAction> const& get();
private:
    static std::vector<ImportTableAction> vect;
};

/**
 * \class ImportTableProcessor
 * Class responsible for moving of data from the Import table into appropriate tables in database.
 */
class ImportTableProcessor
{
public:
    /**
     * Constructor
     * @param db underlying database
     */
    ImportTableProcessor(OsmDatabase& db);
    /**
     * Signal that fires after every action has happened. Parameters specify which action happened and how many
     * rows were affected.
     */
    boost::signal<void (ImportTableAction, int64_t)> action_signal;
    /**
     * Set whether to perform act
     * @param act ImportTableAction in question
     * @param val whether to perform it
     */
    void set(ImportTableAction act, bool val);
    /**
     * Enable act (that is perform it when processing)
     * @param act
     */
    void enable(ImportTableAction act);
    /**
     * Disable act (that is do not perform it when processing)
     * @param act
     */
    void disable(ImportTableAction act);
    /**
     * Enable all actions
     */
    void enable_all();
    /**
     * Disable all actions
     */
    void disable_all();
    /**
     * Set enabled to val for all actions
     * @param val
     */
    void set_all(bool val);
    /**
     * This gets fired just before the real import into tables begins and if any handler returns false, import
     * is cancelled.
     */
    boost::signal<bool (), util::All> proceed_signal;
    /**
     * Process the import table and perform all enabled actions.
     */
    void process();
    virtual ~ImportTableProcessor();
private:
    OsmDatabase& db;
    std::map<ImportTableAction, bool> enabled;
};

} /* namespace osmdb */
#endif /* IMPORTTABLEPROCESSOR_H_ */
