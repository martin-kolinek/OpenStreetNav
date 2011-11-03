#include <exception>
#include <string>

namespace sqlite
{
class InvalidUseException : public std::exception
{
public:
    InvalidUseException(std::string const& msg);
    virtual const char* what() const throw();
    virtual ~InvalidUseException() throw();
private:
    std::string msg;
};
}
