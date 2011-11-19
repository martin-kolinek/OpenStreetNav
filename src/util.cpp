#include "util.h"
#include <iostream>

template<>
double parse<double>(std::string const& str)
{
    return atof(str.c_str());
}

template<>
int64_t parse<int64_t>(std::string const& str)
{
    return atol(str.c_str());
}
