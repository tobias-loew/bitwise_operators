//////////////////////////////////////////////////////////////////
//
//  lunaticpp::flags
//  non-intrusive bitwise operators for flag-like enumerations library
//
//  Copyright Tobias Loew 2023. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see https://github.com/tobias-loew/flags
//


#ifndef FLAGS_HPP_INCLUDED
#define FLAGS_HPP_INCLUDED

#include <type_traits>
#include <boost/config.hpp>
#include <boost/core/underlying_type.hpp>
#include <boost/core/enable_if.hpp>

/////////////////////////////////////////////////////////////////
//
// purpose: enables type-safe bitwise operations on a flag-like enumeration
// usage: to enable bitwise operation for enum "enumeration" define
//
/*

template<>
struct lunaticpp::flags::enable<enumeration> : std::true_type {};

*/
//
// example:
/*

    enum class flags_t {
        a = 0x1,
        b = 0x2,
        c = 0x4,
    };

    enum class flags2_t {
        e = lunaticpp::flags::nth_bit(0), // == 0x1
        f = lunaticpp::flags::nth_bit(1), // == 0x2
        g = lunaticpp::flags::nth_bit(2), // == 0x4
    };

    template<>
    struct lunaticpp::flags::enable<flags_t> : std::true_type {};

    template<>
    struct lunaticpp::flags::enable<flags2_t> : std::true_type {};

    void foo() {
        auto ab = flags_t::a | flags_t::b;  // type of ab is flags_t
        auto bc = flags_t::b | flags_t::c;  // type of bc is flags_t
        auto ab_and_bc = ab & bc;           // type of ab_and_bc is flags_t
        auto not_a = ~flags_t::a;           // type of not_a is lunaticpp::flags::complement<flags_t>
        auto not_not_a = ~not_a;            // type of not_not_a is flags_t

        // auto ae = flags_t::a | flags2_t::e;  // compilation error (different enumerations)


        // test with boolean result
        if (ab_and_bc && not_a) {
            // ...
        }
    }
*/

// consider 
// 
// enum [class] f : underlying_type {
// bit_0 = 0x1      // 2^0
// bit_1 = 0x2      // 2^1
// ...
// bit_{n-1} = 1 << n   // 2^n
// };
// 
// where the underlying_type has m bits (m>=n)
// 
// This library consideres bit_0, ... , bit_{n-1} as the basis of an n-dimensional boolean algebra (BA).
// This BA is closed wrt. the bitwise operations `&` and `|`, but in general (i.e. if m > n) it is not closed wrt. complement `~`,
// as complement also flips bits n, ... , m-1.
// Furthermore, when `s` is a set of options from enum `f`, its complement `~s` generally should *not* be considered as
// a set of options from enum `f` but as its *negation* usually used too diable those options using `&`.
// Thus, lunaticpp::flags differentiates between the BA spanned by the flags specified by `f` and its complement in the BA spanned
// by all bits of the underlying type - the distintion is done by wrapping a `complement`-template aroud the return-type for the complement.
// 


namespace lunaticpp {
    namespace flags {

        // non-intrusive opt-in to operations of lunaticpp::flags
        // specialize
        // `template<> struct lunaticpp::flags::enable<my_enum> : std::true_type {};`
        // to enable operations for scoped or unscoped enums
        template<typename E>
        struct enable : std::false_type {};


        // indicates invalid/incompatible types for operation
        struct error_tag {};

        // explicitly disable error_tag
        template<>
        struct enable<error_tag> : std::false_type {};




        // class-template to indicate complemented flags
        template<typename E>
        struct complement {
            E value;
        };


        // test if E is enabled: either a flags-enum or a negation (detects double-negations)
        template<typename E>
        struct is_enabled;


        // test if E is a flags-enum:
        // detects double-negation
        // returns false_type for non-enabled enums (SFINAE friendly)
        template<typename E>
        struct is_flags;

        // test for complement:
        // detects double-negation
        // returns false_type for non-enabled enums (SFINAE friendly)
        template<typename E>
        struct is_complement;


        // get enum-type from E
        // strip off `complement`-templates
        template<typename E>
        struct enum_type {
            using type = E;
        };

        template<typename E>
        struct enum_type<complement<E>> :enum_type<E> {};

        template<typename E>
        using enum_type_t = typename enum_type<E>::type;


        template<typename E>
        struct is_enabled :enable<enum_type_t<E>> {};


        // test if E is enabled
        template<typename E>
        struct is_flags : is_enabled<E> {};

        template<typename E>
        struct is_flags<complement<E>> : 
            std::conjunction<
                std::negation<is_flags<E>>, 
                is_enabled<E>                       // ensure it's enabled
            > {};


        // test for complement (detects double-negation)
        template<typename E>
        struct is_complement :
            std::conjunction<
                std::negation<is_flags<E>>,
                is_enabled<E>                       // ensure it's enabled
            > {};

        namespace impl {

            // calculate whether result Op(T1) is in the original or the complement boolean algebra
            // always returns the canonical form (either the enum or its negation, never a double negated enum)
            template<typename T1, template<typename> class UnOp>
            struct unary_operation_result {
                using E1 = enum_type_t<T1>;

                using type = typename std::conditional<
                    enable<E1>::value,        // check if undelying enum is enabled
                    typename std::conditional<
                        UnOp<is_complement<T1>>::value,
                        complement<E1>,
                        E1
                    >::type,
                    error_tag
                >::type;
            };


            // calculate whether result Op(T1, T2) is in the original or the complement boolean algebra
            template<typename T1, template<typename> class UnOp>
            struct enable_unary_operation {
                using result_type = typename unary_operation_result<T1, UnOp>::type;

                using type = typename std::enable_if<
                    is_enabled<result_type>::value,
                    result_type
                >::type;
            };


            template<typename T1, template<typename> class UnOp>
            using enable_unary_operation_t = typename enable_unary_operation<T1, UnOp>::type;


            // calculate whether result Op(T1, T2) is in the original or the complement boolean algebra
            // always returns the canonical form (either the enum or its negation, never a double negated enum)
            template<typename T1, typename T2, template<typename, typename> class BinOp>
            struct binary_operation_result {
                using E1 = enum_type_t<T1>;
                using E2 = enum_type_t<T2>;

                using type = typename std::conditional<
                    std::is_same<E1, E2>::value && enable<E1>::value,        // check undelying enums are the same and enabled
                    typename std::conditional<
                        BinOp<is_complement<T1>, is_complement<T2>>::value,
                        complement<E1>,
                        E1
                    >::type,
                    error_tag
                >::type;
            };



            template<typename T1, typename T2, template<typename, typename> class BinOp, bool is_assignment_operator>
            struct hard_error_when_incompatible_type_with_enabled {
                using E1 = enum_type_t<T1>;
                using E2 = enum_type_t<T2>;

                // (enable<E1>::value || enable<E2>::value) -> is_same<E1, E2>::value
                static constexpr bool has_enabled = enable<E1>::value || enable<E2>::value;
                static constexpr bool types_compatible = !has_enabled || std::is_same<E1, E2>::value;

                static_assert(types_compatible, "incompatible type with flags-enabled enum used");

                // When the following static_assert assert fires, then an assignment operator
                // &=, |= or ^= is used, where the lhs-type is not the same as the result-type
                // of the operation (lhs op rhs)
                // e.g.
                // enum e {e0 = 0x1, e1 = 0x2}; template<> enable<e>:std::true_type{};
                // auto v1 = ~e0;       // decltype(v1): complement<e>
                // auto v2 = v1 & e1;   // decltype(v2): e
                // v1 &= e1;            // ill-formed
                static_assert(!is_assignment_operator || !has_enabled || 
                    std::is_same<T1, typename binary_operation_result<T1, T2, BinOp>::type>::value, 
                    "result of binary operation is incompatible with lhs-type of assignment operator");

                static constexpr bool value = true;
            };


            // calculate whether result Op(T1, T2) is in the original or the complement boolean algebra
            template<typename T1, typename T2, template<typename, typename> class BinOp, bool is_assignment_operator>
            struct enable_binary_operation :
                std::enable_if<
                    (hard_error_when_incompatible_type_with_enabled<T1, T2, BinOp, is_assignment_operator>::value,
                    is_enabled<typename binary_operation_result<T1, T2, BinOp>::type>::value),
                    typename std::conditional<is_assignment_operator,
                        T1&,
                        typename binary_operation_result<T1, T2, BinOp>::type
                    >::type
                > 
            {};


            template<typename T1, typename T2, template<typename, typename> class BinOp, bool is_assignment_operator>
            using enable_binary_operation_t = typename enable_binary_operation<T1, T2, BinOp, is_assignment_operator>::type;


            // the standard doesn't provide a not_equal trait, so lets use our own
            template<typename B1, typename B2>
            struct not_equal : std::bool_constant<bool(B1::value) != bool(B2::value)> {};


            template<typename T>
            constexpr auto get_underlying(T value) {
                using underlying = typename boost::underlying_type<T>::type;
                return static_cast<underlying>(value);
            }

            template<typename T>
            constexpr auto get_underlying(complement<T> value) {
                return get_underlying(value.value);
            }

        }


        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
            operator&(T1 lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, std::conjunction, false> {
            using result_t = typename impl::enable_binary_operation_t<T1, T2, std::conjunction, false>;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) & impl::get_underlying(rhs))
            };
        }


        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
            operator|(T1 lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, std::disjunction, false> {
            using result_t = typename impl::enable_binary_operation_t<T1, T2, std::disjunction, false>;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) | impl::get_underlying(rhs))
            };
        }


        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
            operator^(T1 lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, impl::not_equal, false> {
            using result_t = typename impl::enable_binary_operation_t<T1, T2, impl::not_equal, false>;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) ^ impl::get_underlying(rhs))
            };
        }



        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
            operator~(T value) -> typename impl::enable_unary_operation_t<T, std::negation> {
            using result_t = typename impl::enable_unary_operation_t<T, std::negation>;

            return result_t{
                static_cast<enum_type_t<T>>(~impl::get_underlying(value))
            };
        }


        template<typename T1, typename T2>
        constexpr auto
            operator&=(T1& lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, std::conjunction, true> {
            lhs = lhs & rhs;
            return lhs;
        }

        template<typename T1, typename T2>
        constexpr auto
            operator|=(T1& lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, std::disjunction, true> {
            lhs = lhs | rhs;
            return lhs;
        }

        template<typename T1, typename T2>
        constexpr auto
            operator^=(T1& lhs, T2 rhs) -> typename impl::enable_binary_operation_t<T1, T2, impl::not_equal, true> {
            lhs = lhs ^ rhs;
            return lhs;
        }


        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            operator!(T e) {
            return !impl::get_underlying(e);
        }

        // test for == 0 / != 0
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            operator==(T value, std::nullptr_t) {
            return impl::get_underlying(value) == 0;
        }

#if __cplusplus < 202002
        // no rewritten candidates

        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            operator==(std::nullptr_t, T value) {
            return impl::get_underlying(value) == 0;
        }

        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            operator!=(T value, std::nullptr_t) {
            return !(impl::get_underlying(value) == 0);
        }

        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            operator!=(std::nullptr_t, T value) {
            return !(impl::get_underlying(value) == 0);
        }
#endif
        

        // conversion to / from underlying type
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_enabled<T>::value, typename boost::underlying_type<T>::type>::type
            get_underlying(T value) {
            return impl::get_underlying(value);
        }

        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, T>::type
            from_underlying(typename boost::underlying_type<T>::type value) {
            return static_cast<enum_type_t<T>>(value);
        }



        // operators && and ||: convinience functions to do a bitwise & (resp. |) followed by test != 0
        // (a && b) is equivalent to (a & b) != 0
        // (a || b) is equivalent to (a | b) != 0
        //
        // ATTENTION: may change semantics of code with unscoped enums, e.g.
        //
        // enum e {a=1, b=2};
        // if(a && b){...}
        // 
        // with bitwises-operators enabled and BITWISE_OPERATORS_ENABLE_LOGICAL_OPERATORS defined the expression evaluates to false [(a&&b) -> (a & b) != 0 -> 0 !- 0 -> false]
        // while without it evaluates to true [(a&&b) -> (true && true) -> true]

        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
        operator&&(T1 lhs, T2 rhs) -> 
            typename std::enable_if< 
                (impl::hard_error_when_incompatible_type_with_enabled<T1, T2, std::conjunction, false>::value,
                is_enabled<typename impl::binary_operation_result<T1, T2, std::conjunction>::type>::value),
                bool
            >::type {
            return impl::get_underlying(lhs) & impl::get_underlying(rhs);
        }
        
        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr auto
        operator||(T1 lhs, T2 rhs) -> 
            typename std::enable_if< 
                (impl::hard_error_when_incompatible_type_with_enabled<T1, T2, std::disjunction, false>::value,
                is_enabled<typename impl::binary_operation_result<T1, T2, std::disjunction>::type>::value),
                bool
            >::type {
            return impl::get_underlying(lhs) | impl::get_underlying(rhs);
        }
        



        // test if any bit is set
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            any(T e) {
            return impl::get_underlying(e) != 0;
        }

        // test if no bit is set
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, bool>::type
            none(T e) {
            return !e;
        }


        // returns an empty instance of T
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, enum_type_t<T>>::type
            make_null(T) {
            return static_cast<enum_type_t<T>>(0);
        }


        // depending on set
        // returns e or an empty instance of T
        template<typename T>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<is_flags<T>::value, enum_type_t<T>>::type
            make_if(T e, bool set) {
            return set ? e : static_cast<enum_type_t<T>>(0);
        }

        // return a copy of value with all
        // bits of modification set resp. cleared
        template<typename T1, typename T2>
        BOOST_ATTRIBUTE_NODISCARD
        constexpr typename std::enable_if<
            std::is_same<enum_type_t<T1>, enum_type_t<T2>>::value && is_flags<T1>::value && is_flags<T2>::value,
            enum_type_t<T1>
        >::type
            modify(T1 value, T2 modification, bool set) {
            return set ? (value | modification) : (value & ~modification);
        }

        // sets resp. clears the bits of modification
        // in value in-place
        template<typename T1, typename T2>
        constexpr typename std::enable_if<
            std::is_same<enum_type_t<T1>, enum_type_t<T2>>::value && is_flags<T1>::value && is_flags<T2>::value,
            T1&
        >::type
            modify_inplace(T1& value, T2 modification, bool set) {
            value = set ? (value | modification) : (value & ~modification);
            return value;
        }


        // returns a value with the n-th (zero-indexed) bit set
        BOOST_ATTRIBUTE_NODISCARD
        inline constexpr auto nth_bit(unsigned int n) { return 1 << n; }
    }
}


using lunaticpp::flags::operator |;
using lunaticpp::flags::operator &;
using lunaticpp::flags::operator ^;
using lunaticpp::flags::operator ~;
using lunaticpp::flags::operator |=;
using lunaticpp::flags::operator &=;
using lunaticpp::flags::operator ^=;
using lunaticpp::flags::operator !;
using lunaticpp::flags::operator ==;
#if __cplusplus < 202002
using lunaticpp::flags::operator !=;
#endif

using lunaticpp::flags::operator &&;
using lunaticpp::flags::operator ||;


#endif  // FLAGS_HPP_INCLUDED
