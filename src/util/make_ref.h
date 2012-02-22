#ifndef MAKE_REF_H_
#define MAKE_REF_H_

namespace util
{
template<typename T>
T* make_ptr()
{
    return (T*)0;
}

template<typename T>
T& make_ref()
{
    return *make_ptr<T>();
}

template<typename... Args>
std::tuple<Args& ...> make_ref_tuple()
{
    return std::make_tuple(make_ref<Args>()...);
}
}

#endif
