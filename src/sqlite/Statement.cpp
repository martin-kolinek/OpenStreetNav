/*
 * Statement.cpp
 *
 *  Created on: Nov 2, 2011
 *      Author: martin
 */

#include "Statement.h"
#include "SqliteException.h"
#include "InvalidUseException.h"
#include <utility>

namespace sqlite
{

Statement::Statement(const std::string& sql, Database& db):
    stmt(NULL),
    db(&db),
    hrow(false),
    dn(false)
{
    const char* a;
    throw_sqlite_status(sqlite3_prepare_v2(db.cobj(), sql.c_str(), -1, &stmt, &a), db.cobj());
    cols = sqlite3_column_count(stmt);
    db.register_statement(*this);
}

Statement::~Statement()
{
    finalize();
}

Statement& Statement::operator =(Statement && other)
{
    finalize();
    stmt = other.stmt;
    dn = other.dn;
    hrow = other.hrow;
    db = other.db;
    db->unregister_statement(other);
    other.stmt = NULL;
    if (stmt != NULL)
        db->register_statement(*this);
    return *this;
}

Statement::Statement(Statement && other)
    : stmt(NULL),
      db(NULL)
{
    *this = std::move(other);
}

bool Statement::done()
{
    if (stmt == NULL)
        throw InvalidUseException("Call for done after finalization");
    return dn;
}

bool Statement::has_row()
{
    if (stmt == NULL)
        throw InvalidUseException("Call for has_row after finalization");
    return hrow;
}

void Statement::step()
{
    if (stmt == NULL)
        throw InvalidUseException("Call for step after finalization");
    dn = false;
    hrow = false;
    int status = sqlite3_step(stmt);
    if (status == SQLITE_ROW)
    {
        hrow = true;
        dn = false;
        return;
    }
    if (status == SQLITE_DONE)
    {
        reset_internal();
        hrow = false;
        dn = true;
        return;
    }
    if (status == SQLITE_BUSY)
    {
        std::string msg(sqlite3_errmsg(db->cobj()));
        reset_nothrow();
        throw SqliteException(status, msg);
    }
    if (status == SQLITE_MISUSE)
    {
        reset_nothrow();
        throw_sqlite_status(status, db->cobj());
    }
    reset_internal();
}

void Statement::reset()
{
    if (stmt == NULL)
        throw InvalidUseException("Call for reset after finalization");
    if (dn)
    {
        dn = false;
        hrow = false;
    }
    else if (hrow)
    {
        reset_internal();
        hrow = false;
        dn = false;
    }
}

void Statement::reset_internal()
{
    throw_sqlite_status(sqlite3_reset(stmt), db->cobj());
}

void Statement::reset_nothrow()
{
    sqlite3_reset(stmt);
}

const void* Statement::val_blob(int col_index)
{
    check_value_conditions(col_index);
    return sqlite3_column_blob(stmt, col_index);
}

double Statement::val_double(int col_index)
{
    check_value_conditions(col_index);
    return sqlite3_column_double(stmt, col_index);
}

int Statement::val_int(int col_index)
{
    check_value_conditions(col_index);
    return sqlite3_column_int(stmt, col_index);
}

std::string Statement::val_string(int col_index)
{
    check_value_conditions(col_index);
    const unsigned char* ptr = sqlite3_column_text(stmt, col_index);
    const char* cptr = reinterpret_cast<const char*>(ptr);
    return std::string(cptr);
}

void Statement::check_value_conditions(int col)
{
    if (stmt == NULL)
        throw InvalidUseException("Call for value after finalization");
    if (!hrow)
        throw InvalidUseException("Asking for row data with no row available");
    if (col < 0 || col >= cols)
        throw InvalidUseException("Column index out of bounds");
}

void Statement::finalize()
{
    if (stmt == NULL)
        return;
    sqlite3_finalize(stmt);
    db->unregister_statement(*this);
    stmt = NULL;
}

}

