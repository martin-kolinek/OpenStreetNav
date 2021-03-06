/*
 * CompositeSqlCreator.cpp
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#include "CompositeSqlCreator.h"

namespace sqllib
{

sqllib::CompositeSqlCreator::CompositeSqlCreator(std::string const& composite_operator, std::vector<std::shared_ptr<SqlCreator> > children, std::string const& ending):
    op(composite_operator),
    children(children),
    end(ending)
{
}

std::string sqllib::CompositeSqlCreator::create_sql()
{
    std::string ret;
    bool fst = true;
    for (unsigned int i = 0; i < children.size(); ++i)
    {
        if (!fst)
            ret += op + " ";
        fst = false;
        ret += "(" + children[i]->create_sql() + ") ";
    }
    if (children.size() != 0)
        ret += end;
    return ret;
}

} /* namespace sqllib */
