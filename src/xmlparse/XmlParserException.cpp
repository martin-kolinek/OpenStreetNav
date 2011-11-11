/*
 * XmlParserException.cpp
 *
 *  Created on: Nov 10, 2011
 *      Author: martin
 */

#include "XmlParserException.h"

namespace osmxml
{

XmlParserException::XmlParserException(const std::string & msg)
:msg("Xml parser error: "+msg)
{

}

XmlParserException::~XmlParserException() throw()
{
}

const char* XmlParserException::what() const throw()
{
	return msg.c_str();
}

} /* namespace osmxml */
