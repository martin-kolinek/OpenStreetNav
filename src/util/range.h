#ifndef UTIL_RANGE_H_
#define UTIL_RANGE_H_

#include <boost/range/algorithm.hpp>
#include <boost/range/sub_range.hpp>
#include <boost/range/adaptors.hpp>
#include "make_ref.h"

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

template<typename Rng, typename T, typename Func>
auto reduce(Rng const& r, T const& init, Func f) -> decltype(std::accumulate(r.begin(), r.end(), init, f))
{
    return std::accumulate(r.begin(), r.end(), init, f);
}

template<typename Rng>
bool all(Rng const& r)
{
    return reduce(r, true, [](bool a, bool b)
    {
        return a && b;
    });
}

template<typename Rng>
bool any(Rng const& r)
{
    return reduce(r, false, [](bool a, bool b)
    {
        return a || b;
    });
}

template<typename T, typename F>
class TransWrapper
{
public:
    F f;
    TransWrapper(F f):
        f(f)
    {
    }
    typedef decltype(f(util::make_ref<T>())) result_type;
    result_type operator()(T const& t) const
    {
        return f(t);
    }
};

template<typename F>
class SelectedContainer
{
public:
    F f;
    SelectedContainer(F f):
        f(f)
    {
    }
    template<typename Rng>
    auto adapt(Rng const& r) -> decltype(r | boost::adaptors::transformed(TransWrapper<typename boost::range_value<Rng>::type, F>(f))) const
    {
        return r | boost::adaptors::transformed(TransWrapper<typename boost::range_value<Rng>::type, F>(f));
    }
};

template<typename Rng, typename F>
auto operator|(Rng const& r, SelectedContainer<F> s) -> decltype(s.adapt(r))
{
    return s.adapt(r);
}

template<typename F>
SelectedContainer<F> selected(F f)
{
    return SelectedContainer<F>(f);
}

}

#endif
