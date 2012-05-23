#ifndef UTIL_FUNC_H_
#define UTIL_FUNC_H_

namespace util
{

template<typename T>
class IDFunc
{
private:
    T t;
public:
    IDFunc(T const& t):
        t(t)
    {
    }
    typedef T result_type;
    T operator()() const
    {
        return t;
    }
};

template<typename T>
IDFunc<T> id_func(T const& t)
{
    return IDFunc<T>(t);
}

}

#endif
