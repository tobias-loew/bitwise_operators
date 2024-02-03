// Copyright 2024 Tobias Loew.
//
// Distributed under the Boost Software License, Version 1.0.
//
// See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt

#include <boost/core/lightweight_test_trait.hpp>
#include <boost/flags.hpp>

enum 
#ifndef TEST_COMPILE_UNSCOPED
    class
#endif // TEST_COMPILE_UNSCOPED
    flags_enum {
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




void test_nth_bit() {

    BOOST_TEST_EQ(to_underlying(flags_enum::bit_0), 1);
    BOOST_TEST_EQ(to_underlying(flags_enum::bit_1), 2);
    BOOST_TEST_EQ(to_underlying(flags_enum::bit_2), 4);
    BOOST_TEST_EQ(to_underlying(flags_enum::bit_3), 8);

    // test whole range of int
    for (int i = 0; i < sizeof(int) * 8; ++i) {
        BOOST_TEST_EQ(boost::flags::nth_bit(i), 1 << i);
    }
}

void test_negation_operators() {
    static constexpr flags_enum test_cases[] = {
        flags_enum{},
        flags_enum::bit_0,
        flags_enum::bit_1,
        flags_enum::bit_2,
        flags_enum::bit_3,
        flags_enum::bit_0 | flags_enum::bit_1,
        flags_enum::bit_0 | flags_enum::bit_2,
        flags_enum::bit_0 | flags_enum::bit_3,
        flags_enum::bit_0 | flags_enum::bit_1 | flags_enum::bit_2,
        flags_enum::bit_0 | flags_enum::bit_1 | flags_enum::bit_2 | flags_enum::bit_3,
    };

    for (auto value : test_cases) {
        BOOST_TEST(value == ~~value);
        BOOST_TEST(~value == ~~~value);
    }
}

void test_complement_types() {
    flags_enum c0 = flags_enum::bit_0;

    auto c1 = ~c0;
    auto c2 = ~c1;
    auto c3 = ~c2;
    auto c4 = ~c3;
    auto c5 = ~c4;

    static_assert(std::is_same_v<decltype(c1), boost::flags::complement<decltype(c0)>>, "error in complement type");

    static_assert(std::is_same_v<decltype(c0), decltype(c2)>, "error in complement type");
    static_assert(std::is_same_v<decltype(c2), decltype(c4)>, "error in complement type");
    static_assert(std::is_same_v<decltype(c1), decltype(c3)>, "error in complement type");
    static_assert(std::is_same_v<decltype(c3), decltype(c5)>, "error in complement type");

    static_assert(!std::is_same_v<decltype(c0), decltype(c1)>, "error in complement type");

}

void test_normalize_complements() {
    flags_enum c0{};

    boost::flags::complement <
        flags_enum
    > c1{};

    boost::flags::complement<boost::flags::complement<
        flags_enum
        >> c2{};

    boost::flags::complement<boost::flags::complement<boost::flags::complement<
        flags_enum
        >>> c3{};

    boost::flags::complement<boost::flags::complement<boost::flags::complement<boost::flags::complement<
        flags_enum
        >>>> c4{};

    BOOST_TEST(c0 == c2);
    BOOST_TEST(c2 == c4);
    BOOST_TEST(c1 == c3);

    static_assert(std::is_same_v<flags_enum, decltype(~c1)>, "error in complement type");
    static_assert(std::is_same_v<flags_enum, decltype(~c3)>, "error in complement type");
    static_assert(std::is_same_v<decltype(c1), decltype(~c2)>, "error in complement type");
    static_assert(std::is_same_v<decltype(c1), decltype(~c4)>, "error in complement type");
}


void test_binary_operators() {

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;
    flags_enum c = flags_enum::bit_2;
    flags_enum d = flags_enum::bit_3;

    auto ab = a | b;
    auto ac = a | c;
    auto ad = a | d;
    auto acd = a | c | d;
    auto ab_and_ac = ab & ac;
    auto neg_a = ~a;
    auto neg_ac = ~ac;
    auto neg_ad = ~ad;
    auto neg_acd = ~acd;

    auto neg_ad_neg_ac = neg_ad | neg_ac;
    auto neg_ad_and_neg_ac = neg_ad & neg_ac;

    BOOST_TEST(ab_and_ac == a);
    BOOST_TEST((neg_a & d) == d);
    BOOST_TEST((neg_ac & d) == d);
    BOOST_TEST(neg_ad_neg_ac == neg_a);
    BOOST_TEST(neg_ad_and_neg_ac == neg_acd);
    BOOST_TEST(neg_a == (neg_ac|c));

    BOOST_TEST((ac ^ d) == acd);
    BOOST_TEST((acd ^ d) == ac);
}


void test_binary_assignment() {

    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;
    flags_enum c = flags_enum::bit_2;
    flags_enum d = flags_enum::bit_3;

    auto value = a | b;

    BOOST_TEST(value == (a | b));

    value |= c;

    BOOST_TEST(value == (a | b | c));

    value &= ~b;

    BOOST_TEST(value == (a | c));

    value &= (c | d);

    BOOST_TEST(value == (c));

    auto mask = ~value;

    mask ^= value;

    BOOST_TEST(~mask == flags_enum{});

}


void test_null() {
    flags_enum a = flags_enum::bit_0;

    BOOST_TEST((a != nullptr));
    BOOST_TEST(!(a == nullptr));

#if !defined(TEST_COMPILE_UNSCOPED) || defined(TEST_COMPILE_FAIL_UNSCOPED)
    BOOST_TEST((a != 0));
    BOOST_TEST(!(a == 0));
#endif

    BOOST_TEST((a != BF_NULL));
    BOOST_TEST(!(a == BF_NULL));
}

void test_bfand() {
    flags_enum a = flags_enum::bit_0;
    flags_enum b = flags_enum::bit_1;
    flags_enum c = flags_enum::bit_2;
    flags_enum d = flags_enum::bit_3;

    BOOST_TEST(!(a BF_AND b));
    BOOST_TEST(((a | b) BF_AND b));
    BOOST_TEST((a | b) BF_AND (b | c));
    
    BOOST_TEST((a | b) BF_AND (b | c));
    BOOST_TEST(!((a | b) BF_AND (b & c)));
    BOOST_TEST(!((a | b) BF_AND (c | d)));
}



int main() {
    test_nth_bit();
    test_negation_operators();
    test_complement_types();
    test_normalize_complements();
    test_binary_operators();
    test_binary_assignment();
    test_null();
    test_bfand();

    return boost::report_errors();
}