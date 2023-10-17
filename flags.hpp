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

        //enum class options_enum {
        //    disable_order_compare = 0x1,
        //    enable_logical_bool = 0x2,
        //    enable_utility_functions = 0x4,
        //};
        //template<>
        //struct enable<options_enum> : std::true_type {};

        //template<options_enum options_>
        //struct options_t {
        //    static constexpr options_enum options = options_;
        //};

        struct option_disable_logical_bool {};
        struct option_disable_utility_functions {};


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


        template<typename E>
        concept IsFlags = is_flags<E>::value;

        template<typename E>
        concept IsComplement = is_complement<E>::value;

        template<typename E>
        concept IsEnabled = is_enabled<E>::value;


        template<typename T1, typename T2>
        concept IsSameFlags = std::is_same<enum_type_t<T1>, enum_type_t<T2>>::value&&
            IsFlags<T1>&& IsFlags<T2>;


        namespace impl {

            // calculate whether result Op(T1) is in the original or the complemented boolean algebra
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
            // always returns the canonical form (either the enum or its negation, never a double negated enum)
            template<typename T1, typename T2, template<typename...> class BinOp>
            struct binary_operation_result {
                using E1 = enum_type_t<T1>;
                using E2 = enum_type_t<T2>;

                using type = typename std::conditional<
                    std::is_same<E1, E2>::value&& enable<E1>::value,        // check undelying enums are the same and enabled
                    typename std::conditional<
                    BinOp<is_complement<T1>, is_complement<T2>>::value,
                    complement<E1>,
                    E1
                    >::type,
                    error_tag
                >::type;
            };

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

        } // namespace impl


        // concept checking if arguments is enabled
        template<typename T1, template<typename> class UnOp>
        concept UnaryOperationEnabled = is_enabled<typename impl::unary_operation_result<T1, UnOp>::type>::value;

        // concept checking both arguments are compatible and enabled
        template<typename T1, typename T2, template<typename...> class BinOp>
        concept BinaryOperationEnabled = is_enabled<typename impl::binary_operation_result<T1, T2, BinOp>::type>::value;

        // concept checking both arguments are compatible, enabled and the result is assignable to T1
        template<typename T1, typename T2, template<typename...> class BinOp>
        concept BinaryAssignmentEnabled = BinaryOperationEnabled<T1, T2, BinOp>&&
            std::is_same<T1, typename impl::binary_operation_result<T1, T2, BinOp>::type>::value;

        // concept checking both arguments are compatible, enabled and the result is not a complement
        template<typename T1, typename T2, template<typename...> class BinOp>
        concept LogicalOperationEnabled = BinaryOperationEnabled<T1, T2, BinOp>&&
            is_flags<typename impl::binary_operation_result<T1, T2, BinOp>::type>::value;

        template<typename T1, typename T2> 
            requires BinaryOperationEnabled<T1, T2, std::conjunction>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            operator&(T1 lhs, T2 rhs) {
            using result_t = typename impl::binary_operation_result<T1, T2, std::conjunction>::type;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) & impl::get_underlying(rhs))
            };
        }

        template<typename T1, typename T2>
            requires BinaryOperationEnabled<T1, T2, std::disjunction>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            operator|(T1 lhs, T2 rhs) {
            using result_t = typename impl::binary_operation_result<T1, T2, std::disjunction>::type;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) | impl::get_underlying(rhs))
            };
        }

        template<typename T1, typename T2>
            requires BinaryOperationEnabled<T1, T2, impl::not_equal>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            operator^(T1 lhs, T2 rhs) {
            using result_t = typename impl::binary_operation_result<T1, T2, impl::not_equal>::type;

            return result_t{
                static_cast<enum_type_t<T1>>(impl::get_underlying(lhs) ^ impl::get_underlying(rhs))
            };
        }


        template<typename T> 
            requires UnaryOperationEnabled<T, std::negation>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            operator~(T value) {
            using result_t = typename impl::unary_operation_result<T, std::negation>::type;

            return result_t{
                static_cast<enum_type_t<T>>(~impl::get_underlying(value))
            };
        }


        template<typename T1, typename T2> 
            requires BinaryAssignmentEnabled<T1, T2, std::conjunction>
        constexpr T1&
            operator&=(T1& lhs, T2 rhs) {
            lhs = lhs & rhs;
            return lhs;
        }

        template<typename T1, typename T2>
            requires BinaryAssignmentEnabled<T1, T2, std::disjunction>
        constexpr T1&
            operator|=(T1& lhs, T2 rhs) {
            lhs = lhs | rhs;
            return lhs;
        }

        template<typename T1, typename T2>
            requires BinaryAssignmentEnabled<T1, T2, impl::not_equal>
        constexpr T1&
            operator^=(T1& lhs, T2 rhs) {
            lhs = lhs ^ rhs;
            return lhs;
        }


        template<typename T>
            requires IsFlags<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator!(T e) {
            return !impl::get_underlying(e);
        }

        // test for == 0 / != 0
        template<typename T>
            requires IsFlags<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator==(T value, std::nullptr_t) {
            return impl::get_underlying(value) == 0;
        }

#if __cplusplus < 202002
        // no rewritten candidates

        template<typename T>
            requires IsFlags<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator==(std::nullptr_t, T value) {
            return impl::get_underlying(value) == 0;
        }

        template<typename T>
            requires IsFlags<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator!=(T value, std::nullptr_t) {
            return !(impl::get_underlying(value) == 0);
        }

        template<typename T>
            requires IsFlags<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator!=(std::nullptr_t, T value) {
            return !(impl::get_underlying(value) == 0);
        }
#endif


        // conversion to / from underlying type
        template<typename T>
            requires IsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            get_underlying(T value) {
            return impl::get_underlying(value);
        }

        template<typename T>
            requires IsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr auto
            from_underlying(typename boost::underlying_type<enum_type_t<T>>::type value) {
            if constexpr (IsComplement<T>) {
                return complement{ static_cast<enum_type_t<T>>(value) };
            }
            else {
                return static_cast<enum_type_t<T>>(value);
            }
        }

        // end of core part
        //
        ///////////////////////////////////////////////////////////////////////////////////////
        


        template<typename T>
        concept LogicalBoolEnabled =
            !std::is_base_of_v<option_disable_logical_bool, enable<enum_type_t<T>>>;

        template<typename T>
        concept UtilityFunctionsEnabled =
            !std::is_base_of_v<option_disable_utility_functions, enable<enum_type_t<T>>>;

        //
        // operator && : convinience function to do a bitwise & followed by test != 0
        // (a && b) is equivalent to (a & b) != 0
        //
        // ATTENTION: may change semantics of code for unscoped enums, e.g.
        //
        // enum e {a=1, b=2};
        // if(a && b){...}
        //    ^^^^^^
        // with bitwises-operators enabled and flags_ENABLE_LOGICAL_OPERATORS defined
        // the expression evaluates to false [(a&&b) -> (a & b) != 0 -> 0 != 0 -> false]
        // while without it evaluates to true [(a&&b) -> (true && true) -> true]

        template<typename T1, typename T2>
            requires LogicalOperationEnabled<T1, T2, std::conjunction>&& LogicalBoolEnabled<T1>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            operator&&(T1 lhs, T2 rhs) {
            return impl::get_underlying(lhs) & impl::get_underlying(rhs);
        }


        // test if any bit is set
        template<typename T>
            requires IsFlags<T>&& UtilityFunctionsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            any(T e) {
            return impl::get_underlying(e) != 0;
        }

        // test if no bit is set
        template<typename T>
            requires IsFlags<T>&& UtilityFunctionsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr bool
            none(T e) {
            return !e;
        }


        // returns an empty instance of T
        template<typename T>
            requires IsFlags<T>&& UtilityFunctionsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr enum_type_t<T>
            make_null(T) {
            return static_cast<enum_type_t<T>>(0);
        }


        // depending on set
        // returns e or an empty instance of T
        template<typename T>
            requires IsFlags<T>&& UtilityFunctionsEnabled<T>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr enum_type_t<T>
            make_if(T e, bool set) {
            return static_cast<enum_type_t<T>>(set ? impl::get_underlying(e) : 0);
        }

        // return a copy of value with all
        // bits of modification set resp. cleared
        template<typename T1, typename T2>
            requires IsSameFlags<T1, T2>&& UtilityFunctionsEnabled<T1>
        BOOST_ATTRIBUTE_NODISCARD
            constexpr enum_type_t<T1>
            modify(T1 value, T2 modification, bool set) {
            return set ? (value | modification) : (value & ~modification);
        }

        // sets resp. clears the bits of modification
        // in value in-place
        template<typename T1, typename T2>
            requires IsSameFlags<T1, T2> && UtilityFunctionsEnabled<T1>
        constexpr T1&
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


#endif  // FLAGS_HPP_INCLUDED
