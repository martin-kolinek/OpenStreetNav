/*
 * ParsingException.cpp
 *
 *  Created on: Apr 14, 2012
 *      Author: martin
 */

#include "ParsingException.h"

namespace util
{

ParsingException::ParsingException(std::string const& str):
    str("Error parsing value " + str)
{
}

const char* ParsingException::what() const throw()
{
    return str.c_str();
}

ParsingException::~ParsingException() throw()
{
}

} /* namespace util */
