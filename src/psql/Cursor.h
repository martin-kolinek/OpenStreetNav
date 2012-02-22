/*
 * Cursor.h
 *
 *  Created on: Jan 12, 2012
 *      Author: martin
 */

#ifndef PSQLCURSOR_H_
#define PSQLCURSOR_H_

namespace psql
{

class ICursor
{
};

/**
 * \class Cursor
 *
 * Represents a server side cursor. BTypes and RTypes specify types of arguments to statement execution and
 * types of rows returned respectively.
 */
template<typename BTypes, typename RTypes>
class Cursor : ICursor
{
public:
    /**
     * Construct a cursor for given statement
     * @param db database connection to use
     * @param name name of the cursor
     * @param s statement to use
     */
    Cursor(Database& db, std::string const& name, Statement<BTypes, RTypes> const& s, int default_fetch_size = 100000):
        db(&db),
        name(name),
        decl_curs("DECLARE " + name + " CURSOR FOR " + s.get_sql(), db),
        close_curs("CLOSE " + name, db),
        opened(false),
        default_fetch_size(default_fetch_size)
    {
    }
    /**
     * Opens a cursor.
     * Args have to correspond to BTypes
     * @param a arguments to query
     */
    template<typename... Args>
    void open(Args... a)
    {
        if (db == NULL)
            throw PgSqlException("You can't call open on empty cursor");
        decl_curs.execute(a...);
        db->add_cursor(this);
        opened = true;
    }

    /**
     *
     * @return std::vector of last rows fetched
     */
    std::vector<typename RTypes::RowType> const& get_buffer()
    {
        return buffer;
    }

    /**
     * Fetch next count rows from cursor.
     * @param count
     */
    void fetch(int count)
    {
        if (!opened)
            throw PgSqlException("You need to open cursor prior to fetching");
        Statement<BindTypes<>, RTypes > st(util::concatenate(" ", "FETCH", count, "FROM", name), *db);
        buffer = exec_statement(st);
    }

    void fetch()
    {
        fetch(default_fetch_size);
    }

    virtual ~Cursor()
    {
        close();
    }
    Cursor(Cursor const&) = delete;
    Cursor& operator=(Cursor const&) = delete;
    /**
     * Constructs an empty cursor.
     */
    Cursor():
        db(NULL),
        opened(false)
    {
    }
    /**
     * Move constructs a Cursor from other Cursor
     * @param other
     */
    Cursor(Cursor && other):
        db(NULL),
        opened(false)
    {
        *this = std::move(other);
    }
    /**
     * Move assigns from other Cursor
     * @param other
     * @return *this
     */
    Cursor& operator=(Cursor && other)
    {
        if (this == &other)
            return *this;
        close();
        decl_curs = std::move(other.decl_curs);
        close_curs = std::move(other.close_curs);
        name = std::move(other.name);
        db = other.db;
        other.db = NULL;
        opened = other.opened;
        buffer = std::move(other.buffer);
        default_fetch_size = other.default_fetch_size;
        return *this;
    }

    /**
     * Closes the cursor.
     */
    void close()
    {
        if (db != NULL && opened && db->in_transaction() && db->is_cursor(this))
        {
            close_curs.execute();
            db->remove_cursor(this);
        }
        opened = false;
    }
private:
    Database* db;
    std::string name;
    Statement<BTypes, RetTypes<> > decl_curs;
    Statement<BindTypes<>, RetTypes<> > close_curs;
    std::vector<typename RTypes::RowType> buffer;
    bool opened;
    int default_fetch_size;
};

/**
 * Crates a cursor from a statement. This is a utility function which saves you from specifying BTypes and RTypes
 * to Cursor constructor.
 * @param db database connection
 * @param name cursor name
 * @param st statement
 * @return Cursor for st
 */
template<typename BTypes, typename RTypes>
Cursor<BTypes, RTypes> make_cursor(Database& db, std::string const& name, Statement<BTypes, RTypes> const& st)
{
    return Cursor<BTypes, RTypes>(db, name, st);
}

} /* namespace psql */
#endif /* PSQLCURSOR_H_ */
