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
        for (; first != last; ++first)
        {
            ret = ret && *first;
        }
        return ret;
    }
};

std::string replace(std::string const& input, std::map<char, std::string> const& repl);

template<typename Less, typename Eq, typename A, typename B>
bool greater_than_impl(A const& a, B const& b)
{
    Less l;
    Eq e;
    return !e(a, b) && !l(a, b);
}

template<typename Eq, typename A, typename B>
bool not_eq_impl(A const& a, B const& b)
{
    Eq e;
    return !e(a, b);
}

template<typename Less, typename A, typename B>
bool greater_eq_impl(A const& a, B const& b)
{
    Less l;
    return !l(a, b);
}

template<typename Less, typename Eq, typename A, typename B>
bool less_eq_impl(A const& a, B const& b)
{
    Less l;
    Eq e;
    return e(a, b) || l(a, b);
}

template<typename A>
bool greater_than_impl(A const& a, A const& b)
{
    return greater_than_impl<std::less<A>, std::equal_to<A>, A, A>(a, b);
}

template<typename A>
bool not_eq_impl(A const& a, A const& b)
{
    return not_eq_impl<std::equal_to<A>, A, A>(a, b);
}

template<typename A>
bool greater_eq_impl(A const& a, A const& b)
{
    return greater_eq_impl<std::less<A>, A, A>(a, b);
}

template<typename A>
bool less_eq_impl(A const& a, A const& b)
{
    return less_eq_impl<std::less<A>, std::equal_to<A>, A, A>(a, b);
}

template<typename Eq, typename Col1, typename Col2>
bool equal_collection(Col1 c1, Col2 c2)
{
    return std::equal<decltype(c1.begin()), decltype(c2.begin()), Eq>(c1.begin(), c1.end(), c2.begin(), Eq());
}

template<typename Col1, typename Col2>
bool equal_collection(Col1 c1, Col2 c2)
{
    typedef decltype(*c1.begin()) Elem;
    return equal_collection<Col1, Col2, std::equal_to<Elem> >(c1, c2);
}

}

#endif /* UTIL_H_ */
