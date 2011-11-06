/*
 * WrongDBException.h
 *
 *  Created on: Nov 6, 2011
 *      Author: martin
 */

#ifndef WRONGDBEXCEPTION_H_
#define WRONGDBEXCEPTION_H_

#include <exception>
#include <string>

namespace osmdb
{

class WrongDBException : public std::exception
{
public:
    WrongDBException(std::string const& filename);
    virtual ~WrongDBException() throw ();
    virtual const char* what() const throw();
private:
    std::string filename;
};

} /* namespace osmdb */
#endif /* WRONGDBEXCEPTION_H_ */
