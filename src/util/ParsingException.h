/*
 * ParsingException.h
 *
 *  Created on: Apr 14, 2012
 *      Author: martin
 */

#ifndef PARSINGEXCEPTION_H_
#define PARSINGEXCEPTION_H_

#include <exception>
#include <string>

namespace util
{

class ParsingException : std::exception
{
public:
    ParsingException(std::string const& str);
    const char* what() const throw();
    virtual ~ParsingException() throw();
private:
    std::string str;
};

} /* namespace util */
#endif /* PARSINGEXCEPTION_H_ */
