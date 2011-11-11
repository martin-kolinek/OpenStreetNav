/*
 * XmlParserException.h
 *
 *  Created on: Nov 10, 2011
 *      Author: martin
 */

#ifndef XMLPARSEREXCEPTION_H_
#define XMLPARSEREXCEPTION_H_

#include <string>
#include <exception>

namespace osmxml
{

class XmlParserException : public std::exception
{
public:
	XmlParserException(std::string const& msg);
	virtual ~XmlParserException() throw();
	virtual const char* what() const throw();
private:
	std::string msg;
};

} /* namespace osmxml */
#endif /* XMLPARSEREXCEPTION_H_ */
