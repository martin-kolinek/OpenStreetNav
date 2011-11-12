/*
 * util.h
 *
 *  Created on: Nov 8, 2011
 *      Author: martin
 */

#ifndef UTIL_H_
#define UTIL_H_

#include <istream>
#include <sstream>

template<typename T>
T parse(std::string const& str)
{
    T ret;
    std::istringstream ss(str);
    ss >> ret;
    if (!ss.eof())
        throw std::exception();//TODO
    return ret;
}

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

#endif /* UTIL_H_ */
