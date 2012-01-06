/*
 * CompositeSqlCreator.h
 *
 *  Created on: Dec 5, 2011
 *      Author: martin
 */

#ifndef COMPOSITESQLCREATOR_H_
#define COMPOSITESQLCREATOR_H_

#include "SqlCreator.h"
#include <vector>
#include <memory>

namespace sqllib
{

class CompositeSqlCreator : public SqlCreator
{
public:
    CompositeSqlCreator(std::string const& composite_operator, std::vector<std::shared_ptr<SqlCreator> > children, std::string const& ending = "");
    std::string create_sql();
private:
    std::string op;
    std::vector<std::shared_ptr<SqlCreator> > children;
    std::string end;
};

} /* namespace sqllib */
#endif /* COMPOSITESQLCREATOR_H_ */
