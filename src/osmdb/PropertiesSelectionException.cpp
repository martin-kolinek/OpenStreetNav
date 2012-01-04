/*
 * FillException.cpp
 *
 *  Created on: Jan 3, 2012
 *      Author: martin
 */

#include "PropertiesSelectionException.h"

namespace osmdb
{

PropertiesSelectionException::PropertiesSelectionException(const std::string& problem)
    : msg(problem)
{
}

PropertiesSelectionException::~PropertiesSelectionException() throw()
{
}

const char* PropertiesSelectionException::what() const throw()
{
    return msg.c_str();
}

} /* namespace osmdb */
