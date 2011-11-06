/*
 * WrongDBException.cpp
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#include "WrongDBException.h"
#include <sstream>
#include <ostream>

namespace osmdb
{

WrongDBException::WrongDBException(const std::string& filename)
    : filename(filename)
{
}

WrongDBException::~WrongDBException() throw()
{
}

const char* WrongDBException::what() const throw()
{
    std::ostringstream ss;
    ss << "Database validation failed " << filename;
    return ss.str().c_str();
}



} /* namespace osmdb */
