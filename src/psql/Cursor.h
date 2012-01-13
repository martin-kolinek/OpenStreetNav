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

template<typename BTypes, typename RTypes>
class Cursor : ICursor
{
public:
    Cursor(Database& db, std::string const& name, Statement<BTypes, RTypes> const& s):
        db(&db),
        name(name),
        decl_curs("DECLARE " + name + " CURSOR FOR " + s.get_sql(), db),
        close_curs("CLOSE " + name, db),
        opened(false)
    {
    }
    template<typename... Args>
    void open(Args... a)
    {
        if (db == NULL)
            throw PgSqlException("You can't call open on empty cursor");
        decl_curs.execute(a...);
        db->add_cursor(this);
        opened = true;
    }
    std::vector<typename RTypes::RowType> const& get_buffer()
    {
        return buffer;
    }
    void fetch(int count)
    {
        if (!opened)
            throw PgSqlException("You need to open cursor prior to fetching");
        Statement<BindTypes<>, RTypes > st(util::concatenate(" ", "FETCH", count, "FROM", name), *db);
        buffer = exec_statement(st);
    }
    virtual ~Cursor()
    {
        close();
    }
    Cursor(Cursor const&) = delete;
    Cursor& operator=(Cursor const&) = delete;
    Cursor():
        db(NULL),
        opened(false)
    {
    }
    Cursor(Cursor && other):
        db(NULL),
        opened(false)
    {
        *this = std::move(other);
    }
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
        return *this;
    }

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
};

template<typename BTypes, typename RTypes>
Cursor<BTypes, RTypes> make_cursor(Database& db, std::string const& name, Statement<BTypes, RTypes> const& st)
{
    return Cursor<BTypes, RTypes>(db, name, st);
}

} /* namespace psql */
#endif /* PSQLCURSOR_H_ */
