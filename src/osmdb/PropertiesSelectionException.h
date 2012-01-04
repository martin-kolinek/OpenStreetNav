/*
 * FillException.h
 *
 *  Created on: Jan 3, 2012
 *      Author: martin
 */

#ifndef PROPERTIES_SELECTION_EXCEPTION_H_
#define PROPERTIES_SELECTION_EXCEPTION_H_

#include <exception>
#include <string>

namespace osmdb
{

class PropertiesSelectionException : public std::exception
{
public:
    PropertiesSelectionException(std::string const& problem);
    virtual ~PropertiesSelectionException() throw ();
    const char* what() const throw();
private:
    std::string msg;
};

} /* namespace osmdb */
#endif /* PROPERTIES_SELECTION_EXCEPTION_H_ */
