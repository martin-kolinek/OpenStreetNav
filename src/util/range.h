#ifndef UTIL_RANGE_H_
#define UTIL_RANGE_H_

#include <boost/range/algorithm.hpp>
#include <boost/range/sub_range.hpp>

namespace util
{
template<typename Rng>
size_t count(Rng const& r)
{
    return std::distance(r.begin(), r.end());
}

template<typename Rng, typename Pred>
auto split(Rng& rng, Pred pred = Pred()) -> decltype(std::make_pair(boost::sub_range<Rng>(rng.begin(), boost::range::find_if(rng, pred)), boost::sub_range<Rng>(boost::range::find_if(rng, pred), rng.end())))
{

    auto it = boost::range::find_if(rng, pred);
    auto it2 = it;
    if (it != rng.end())
        it2++;

    return std::make_pair(boost::sub_range<Rng>(rng.begin(), it2), boost::sub_range<Rng>(it, rng.end()));

}

template<typename... Rest>
std::vector<std::string> gen_vect(const char* t, Rest const& ... rst)
{
    std::vector<std::string> res;
    res.reserve(sizeof...(rst));
    fill_vect(res, t, rst...);
    return res;
}

template<typename T, typename... Rest>
std::vector<T> gen_vect(T const& t, Rest const& ... rst)
{
    std::vector<T> res;
    res.reserve(sizeof...(rst));
    fill_vect(res, t, rst...);
    return res;
}

template<typename T>
void fill_vect(std::vector<T>&)
{
}

template<typename... Rest>
void fill_vect(std::vector<std::string>& v, const char* t1, Rest const& ... ts)
{
    v.push_back(t1);
    fill_vect(v, ts...);
}

template<typename T, typename... Rest>
void fill_vect(std::vector<T>& v, T const& t1, Rest const& ... ts)
{
    v.push_back(t1);
    fill_vect(v, ts...);
}

}

#endif
