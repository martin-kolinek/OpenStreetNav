#include "util.h"
#include <iostream>

namespace util
{

std::string replace(std::string const& input, std::map<char, std::string> const& repl)
{
    std::ostringstream stream;
    for (auto it = input.begin(); it != input.end(); ++it)
    {
        auto rep_it = repl.find(*it);
        if (rep_it != repl.end())
            stream << rep_it->second;
        else
            stream << *it;
    }
    return stream.str();
}

}
