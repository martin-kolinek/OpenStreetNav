#include "InvalidUseException.h"

namespace sqlite
{

InvalidUseException::InvalidUseException(std::string const& msg):
    msg(msg)
{
}

InvalidUseException::~InvalidUseException() throw()
{
}

char const* InvalidUseException::what() const throw()
{
    return ("Invalid use of sqlite: " + msg).c_str();
}

}
