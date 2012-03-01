#ifndef UTIL_RANGE_H_
#define UTIL_RANGE_H_

namespace util
{
template<typename Rng>
size_t count(Rng const& r)
{
    return std::distance(r.begin(), r.end());
}
}

#endif
