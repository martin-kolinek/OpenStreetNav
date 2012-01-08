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

namespace util
{

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

template<typename T1, typename T2>
class dereferenced_equal_to : std::binary_function <T1, T2, bool>
{
public:
    bool operator()(T1 const& a, T2 const& b)
    {
        return *a == *b;
    }
};

template<typename T1, typename T2>
dereferenced_equal_to<T1, T2> get_dereferenced_equal_to(T1 const&, T2 const&)
{
    return dereferenced_equal_to<T1, T2>();
}

template < typename Eq, typename K, typename V, typename Compare = std::less<K>, typename Allocator = std::allocator<std::pair<const K, V> > >
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

template < typename K, typename V, typename Compare = std::less<K>, typename Allocator = std::allocator<std::pair<const K, V> > >
bool multimap_eq(std::multimap<K, V, Compare, Allocator> const& a, std::multimap<K, V, Compare, Allocator> const& b)
{
    return multimap_eq<std::equal_to<K> >(a, b);
}

template <typename Eq, typename It, typename T>
It find(It first, It last, T const& value)
{
    for ( ; first != last; first++) if ( Eq()(*first, value) ) break;
    return first;
}

template <typename It, typename T>
It find(It first, It last, T const& value)
{
    return find<std::equal_to<T> >(first, last, value);
}

class All
{
public:
	typedef bool result_type;
	template<typename It> bool operator()(It first, It last) const
	{
		bool ret = true;
		for(; first!=last; ++first)
		{
			ret= ret && *first;
		}
		return ret;
	}
};

}

#endif /* UTIL_H_ */
