/*
 * seq.h
 *
 *  Created on: Feb 26, 2012
 *      Author: martin
 */

#ifndef SEQ_H_
#define SEQ_H_

namespace util
{

template<int... S>
class seq
{
};

template<int N, int... S>
class gen : public gen < N - 1, N - 1, S... >
{

};

template<int... S>
class gen<0, S...>
{
public:
    typedef seq<S...> type;
};

template<int S>
auto gen_seq() -> typename gen<S>::type
{
    typename gen<S>::type s;
    return s;
}

}

#endif /* SEQ_H_ */
