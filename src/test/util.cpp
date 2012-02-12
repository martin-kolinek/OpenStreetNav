#include <boost/test/unit_test.hpp>

#include "../util/util.h"
#include "../util/ConcatCollection.h"
#include "../util/unpack_call.h"
#include "../util/RowDataDeserializer.h"
#include "../util/func.h"
#include <algorithm>

BOOST_AUTO_TEST_SUITE(util)

BOOST_AUTO_TEST_CASE(concat)
{
    BOOST_CHECK_EQUAL("1 2 3", concatenate(" ", 1, 2, 3));
}

BOOST_AUTO_TEST_CASE(mult_eq)
{
    std::multimap<int, int> mp;
    mp.insert(std::pair<int, int>(1, 2));
    mp.insert(std::pair<int, int>(1, 3));

    std::multimap<int, int> mp2;
    mp2.insert(std::pair<int, int>(1, 3));
    mp2.insert(std::pair<int, int>(1, 2));
    multimap_eq(mp, mp2);
}

BOOST_AUTO_TEST_CASE(concat_it)
{
    std::vector<int> v1 {1, 2, 3, 4};
    std::vector<int> v2 {5, 6, 7, 8};
    auto coll = util::make_concat_coll(v1.begin(), v1.end(), v2.begin(), v2.end());
    std::vector<int> v {1, 2, 3, 4, 5, 6, 7, 8};
    BOOST_CHECK(std::equal(coll.begin(), coll.end(), v.begin()));
}

int test(int a, int b, int c)
{
	return a+b+c;
}

BOOST_AUTO_TEST_CASE(unpack_call_test)
{
	auto tup = std::make_tuple(1, 2, 3);
	auto i = util::unpack_call(test, tup);
	BOOST_CHECK(i==6);
}

void test1(int a)
{
	a=2;
}

BOOST_AUTO_TEST_CASE(rowdatadeserializer)
{
	std::vector<std::tuple<int> > vect{std::make_tuple(1)};
	util::deserialize_collection(vect.begin(), vect.end(), true, &test1);
}

int test2(int a, int b)
{
	return a+b;
}

BOOST_AUTO_TEST_CASE(bind1st_test)
{
	auto ptr = util::bind1st(test2, 3);
	BOOST_CHECK(ptr(3)==6);
}

class Functor
{
public:
	int operator()(int a)
	{
		return a;
	}
};

BOOST_AUTO_TEST_CASE(bind1st_functor)
{
	Functor f;
	auto ptr = util::bind1st(f, 3);
	BOOST_CHECK(ptr()==3);
}

class A
{
public:
	int test(int a)
	{
		return a;
	}
};

BOOST_AUTO_TEST_CASE(bind1st_class)
{
	A a;
	auto f = bind1st(&A::test, &a);
	BOOST_CHECK(f(3)==3);
}

BOOST_AUTO_TEST_CASE(bind1st_unpack_call)
{
	A a;
	auto f = bind1st(&A::test, &a);
	int b = unpack_call<bind_class<single_mem_fn<int, A, int> >, int>(f, std::make_tuple(3));
	BOOST_CHECK(b==3);
}

BOOST_AUTO_TEST_SUITE_END()
