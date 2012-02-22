#ifndef UTIL_FUNC_H_
#define UTIL_FUNC_H

#include <tuple>

namespace util
{

template<typename Func>
class member_fun_ptr_traits
{
};

template<typename Ret, typename T, typename... Args>
class member_fun_ptr_traits<Ret (T::*)(Args...)>
{
public:
    typedef Ret result_type;
    typedef std::tuple<Args...> arg_tuple;
};

template<typename Func>
class function_traits : public member_fun_ptr_traits<decltype(&Func::operator())>
{
};

template<typename Ret, typename... Args>
class function_traits<Ret (*)(Args...)>
{
public:
    typedef Ret result_type;
    typedef std::tuple<Args...> arg_tuple;
};

template<typename Ret, typename... Args>
class function_traits<Ret (&)(Args...)>
{
public:
    typedef Ret result_type;
    typedef std::tuple<Args...> arg_tuple;
};

template<typename Ret, typename T, typename... Args>
class function_traits<Ret (T::*) (Args...)>
{
public:
    typedef Ret result_type;
    typedef std::tuple<T, Args...> arg_tuple;
};

template<typename Func>
class bind_class
{
public:
    typedef typename std::tuple_element<0, typename function_traits<Func>::arg_tuple>::type first_arg;
    bind_class(Func f, first_arg first):
        f(f),
        first(first)
    {
    }
private:
    Func f;
    first_arg first;
public:
    typedef typename function_traits<Func>::result_type result_type;
    template<typename... Args>
    result_type operator()(Args const& ... args)
    {
        return f(first, args...);
    }
};

template<typename Func>
bind_class<Func> bind1st(Func f, typename bind_class<Func>::first_arg const& first)
{
    return bind_class<Func>(f, first);
}

template<typename Ret, typename T, typename... Args>
class single_mem_fn
{
private:
    typedef decltype(std::mem_fn((Ret (T::*)(Args...))0)) mf_t;
    mf_t mf;
public:
    single_mem_fn(Ret (T::*f)(Args...)):
        mf(std::mem_fn(f))
    {
    }

    Ret operator()(T* t, Args... a)
    {
        return mf(t, a...);
    }
};

template<typename Ret, typename T, typename... Args>
auto bind1st(Ret (T::*f)(Args...), T* first) -> bind_class<single_mem_fn<Ret, T, Args...> >
{
    return bind_class<single_mem_fn<Ret, T, Args...> >(single_mem_fn<Ret, T, Args...>(f), first);
}

}

#endif
