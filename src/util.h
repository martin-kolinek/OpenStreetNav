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
#include <map>

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
double parse<double>(std::string const& str);

template<>
int64_t parse<int64_t>(std::string const& str);

template<typename Sep, typename... Args>
std::string concatenate(Sep sep, Args... args)
{
    std::ostringstream ss;
    concat_impl(ss, sep, args...);
    return ss.str();
}

template<typename T>
std::string to_str(T val)
{
    return concatenate("", val);
}

template<typename Sep, typename Tail>
void concat_impl(std::ostringstream& ss, Sep, Tail t)
{
    ss << t;
}

template<typename Sep, typename Head, typename... Tail>
void concat_impl(std::ostringstream& os, Sep sep, Head h, Tail... t)
{
    os << h << sep;
    concat_impl(os, sep, t...);
}

template < typename K, typename V, typename Compare = std::less<K>, typename Allocator = std::allocator<std::pair<const K, V> >, typename Eq = std::equal_to<K> >
bool multimap_eq(std::multimap<K, V, Compare, Allocator> const& a, std::multimap<K, V, Compare, Allocator> const& b)
{
    for (auto it = a.begin(); it != a.end(); ++it)
    {
        bool wrong = true;
        for (auto it2 = b.lower_bound(it->first); it2 != b.upper_bound(it->first); ++it2)
        {
            if (Eq()(it->second, it2->second))
                wrong = false;
        }
        if (wrong)
            return false;
    }
    return true;
}


#endif /* UTIL_H_ */
