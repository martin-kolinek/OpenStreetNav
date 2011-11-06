/*
 * Database.h
 *
 *  Created on: Nov 2, 2011
 *      Author: martin
 */

#ifndef DATABASE_H_
#define DATABASE_H_

#include <string>
#include <sqlite3.h>
#include <set>

namespace sqlite
{

class Statement;

/**
 * \class Database
 *
 * Database connection class. Disconnects when destroyed (see \ref force_close).
 */
class Database
{
public:
    /**
     * Constructs the database connection object.
     * @param filename
     */
    Database(std::string const& filename);
    virtual ~Database();
    Database& operator=(Database const&) = delete;
    Database(Database const&) = delete;
    Database& operator=(Database && other);
    Database(Database && other);
    /**
     * Tries to disconnect. Throws InvalidUseException if any unfinalized statements exist.
     */
    void close();
    /**
     * Disconnects. Any unfinalized statements are finalized before that.
     */
    void force_close() throw();
    /**
     * Registers a Statement. As Statement class does this automatically so you shoudln't have to call this
     * function
     * @param st Statement to be registered
     */
    void register_statement(Statement& st);
    /**
     * Unregisters a Statement. As Statement class does this automatically so you shoudln't have to call this
     * function
     * @param st Statement to be unregistered.
     */
    void unregister_statement(Statement& st);
    /**
     *
     * @return number of unfinalized statements
     */
    int unfinalized();
    sqlite3* const& cobj();
private:
    sqlite3* conn;
    std::set<Statement*> stmts;
};
}
#endif /* DATABASE_H_ */
