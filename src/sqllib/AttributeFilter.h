class Container
{
private:
    std::string select;
    std::string from;
    std::string where;
    std::string ident;
public:
    Container(std::string const& select, std::string const& from, std::string const& where, std::string const& ident):
        select(select),
        from(from),
        where(where),
        ident(ident)
    {}
    std::string get_select(std::string const& key, std::string const& value, std::string ident)
    {
        return "SELECT " + select + ", " + ident + " FROM " + from + " WHERE " + where + " AND " + ident + ".key='" + key + "' AND " + ident + ".value='" + value + "'";
    }
};

class Element
{
public:
    virtual std::string get_select() = 0;
};

class ConnectElement
{
public:
    std::string get_select(Container const& cont)
    {
    }
}

class AttributeFilter
{
private:
    Container cont;
public:
    std::string get_select(
};
