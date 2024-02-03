// Copyright 2024 Tobias Loew.
//
// Distributed under the Boost Software License, Version 1.0.
//
// See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt

#include <boost/core/lightweight_test_trait.hpp>
#include <boost/flags.hpp>

enum class flags_enum {
    bit_0 = boost::flags::nth_bit(0), // == 0x01
    bit_1 = boost::flags::nth_bit(1), // == 0x02
    bit_2 = boost::flags::nth_bit(2), // == 0x04
    bit_3 = boost::flags::nth_bit(3), // == 0x08
};

// enable flags_enum
template<> struct boost::flags::enable<flags_enum> : std::true_type {};


// helpers
template<typename E>
constexpr auto to_underlying(E value) {
    return static_cast<std::underlying_type_t<E>>(value);
}



void test_any() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(any(a));
    BOOST_TEST(any(a | b));
    BOOST_TEST(!any(a & b));

#ifdef TEST_COMPILE_FAIL_COMPLEMENT_ANY
    auto v = any(~a);
#endif
}


void test_none() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(!none(a));
    BOOST_TEST(!none(a | b));
    BOOST_TEST(none(a & b));

#ifdef TEST_COMPILE_FAIL_COMPLEMENT_NONE
    auto v = none(~a);
#endif
}

void test_contains() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(contains(a, flags_enum{}));
    BOOST_TEST(!contains(flags_enum{}, a));
    BOOST_TEST(!contains(a, a | b));
    BOOST_TEST(contains(a | b, a | b));
    BOOST_TEST(contains(a | b, a));
    BOOST_TEST(!contains(b, a));
}

void test_intersect() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(!intersect(a, flags_enum{}));
    BOOST_TEST(!intersect(flags_enum{}, a));
    BOOST_TEST(intersect(a, a | b));
    BOOST_TEST(intersect(a | b, a | b));
    BOOST_TEST(intersect(a | b, a));
    BOOST_TEST(!intersect(b, a));
}

void test_disjoint() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(disjoint(a, flags_enum{}));
    BOOST_TEST(disjoint(flags_enum{}, a));
    BOOST_TEST(!disjoint(a, a | b));
    BOOST_TEST(!disjoint(a | b, a | b));
    BOOST_TEST(!disjoint(a | b, a));
    BOOST_TEST(disjoint(b, a));
}

void test_make_null() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;

    BOOST_TEST(make_null(flags_enum{}) == flags_enum{});
    BOOST_TEST(make_null(a) == flags_enum{});
}


void test_make_if() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;

    BOOST_TEST(make_if(a, false) == flags_enum{});
    BOOST_TEST(make_if(a, true) == a);
}


void test_modify() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    BOOST_TEST(modify(a, b, false) == a);
    BOOST_TEST(modify(a, b, true) == (a | b));
}


void test_modify_inplace() {
    using namespace boost::flags;

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;

    {
        auto v = a;
        modify_inplace(v, b, false);
        BOOST_TEST(v == a);
    }

    {
        auto v = a;
        modify_inplace(v, b, true);
        BOOST_TEST(v == (a | b));
    }
}


int main() {
    test_any();
    test_none();
    test_contains();
    test_intersect();
    test_disjoint();
    test_make_null();
    test_make_if();
    test_modify();
    test_modify_inplace();

    return boost::report_errors();
}