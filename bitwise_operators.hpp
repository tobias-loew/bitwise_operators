//////////////////////////////////////////////////////////////////
//
// bitwise operators library
//
//  Copyright Tobias Loew 2020. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see https://github.com/tobias-loew/bitwise_operators
//


#ifndef BITWISE_OPERATORS_HPP_INCLUDED
#define BITWISE_OPERATORS_HPP_INCLUDED

#include<type_traits>

/////////////////////////////////////////////////////////////////
//
// purpose: enables type-safe bitwise operations on a flag-like enumeration
// usage: to enable bitwise operation for enum "enumeration" define
//
/*

template<>
struct boost::bitwise_operators::enable_operators<enumeration> : std::true_type {};

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
        e = boost::bitwise_operators::nth_bit(0), // == 0x1
        f = boost::bitwise_operators::nth_bit(1), // == 0x2
        g = boost::bitwise_operators::nth_bit(2), // == 0x4
    };

    template<>
    struct boost::bitwise_operators::enable_operators<flags_t> : std::true_type {};

    template<>
    struct boost::bitwise_operators::enable_operators<flags2_t> : std::true_type {};

    void foo() {
        auto ab = flags_t::a | flags_t::b;  // type of ab is flags_t
        auto bc = flags_t::b | flags_t::c;  // type of bc is flags_t
        auto ab_and_bc = ab & bc;           // type of ab_and_bc is flags_t
        auto not_a = ~flags_t::a;           // type of not_a is boost::bitwise_operators::bitmask<flags_t>
        auto not_not_a = ~not_a;            // type of not_not_a is flags_t

        // auto ae = flags_t::a | flags2_t::e;  // compilation error


        // test with boolean result
        if (ab_and_bc && not_a) {
            // ...
        }
    }
*/

namespace boost {
    namespace bitwise_operators {

        template<typename E>
        struct enable_operators : std::false_type {};

        template<typename E>
        struct bitmask {
            E mask;
        };



        namespace impl {
            struct tag_different_types_used {};
        }

        // explicitely specialize enable_operators for impl::tag_different_types_used to prevent users from doing different
        template<>
        struct enable_operators<impl::tag_different_types_used > : std::false_type {};

        namespace impl {
            template<typename E>
            struct get_enum {
                using type = E;
            };

            template<typename E>
            struct get_enum<bitmask<E>> :get_enum<E> {};

            template<typename E>
            using get_enum_t = typename get_enum<E>::type;


            template<typename E>
            struct is_bitmask : std::false_type {};

            template<typename E>
            struct is_bitmask<bitmask<E>> : std::true_type {};

            template<typename E>
            struct is_enabled_for_operators : enable_operators<E> {};

            template<typename E>
            struct is_enabled_for_operators< bitmask<E>> : is_enabled_for_operators<E> {};


            template<typename E>
            using enable_operator_t = std::enable_if_t<is_enabled_for_operators<E>::value, E>;

            template<typename E1, typename E2, template<class...> class Op>
            struct bitmask_operation_result {
                using type = std::conditional_t<
                    std::is_same_v<get_enum_t<E1>, get_enum_t<E2>>,
                    std::conditional_t<
                    Op<is_bitmask<E1>, is_bitmask<E2>>::value,
                    bitmask<get_enum_t<E1>>,
                    get_enum_t<E1>
                    >,
                    tag_different_types_used
                >;
            };

            template<typename E1, typename E2>
            using bitmask_operation_and_result_t = typename bitmask_operation_result<E1, E2, std::conjunction>::type;

            template<typename E1, typename E2>
            using bitmask_operation_or_result_t = typename bitmask_operation_result<E1, E2, std::disjunction>::type;


            template<typename B1, typename B2>
            struct xoring : std::bool_constant<B1::value^ B2::value> {};

            template<typename E1, typename E2>
            using bitmask_operation_xor_result_t = typename bitmask_operation_result<E1, E2, xoring>::type;


            template<typename E>
            constexpr auto get_underlying(E value) {
                using underlying = typename std::underlying_type<E>::type;
                return static_cast<underlying>(value);
            }

            template<typename E>
            constexpr auto get_underlying(bitmask<E> value) {
                return get_underlying(value.mask);
            }

        }


        template<typename E1, typename E2>
        [[nodiscard]]
        constexpr auto
            operator|(E1 lhs, E2 rhs) -> impl::enable_operator_t<impl::bitmask_operation_or_result_t<E1, E2>> {
            using result_t = typename impl::enable_operator_t<impl::bitmask_operation_or_result_t<E1, E2>>;

            return result_t{
                static_cast<impl::get_enum_t<E1>>(impl::get_underlying(lhs) | impl::get_underlying(rhs))
            };
        }


        template<typename E1, typename E2>
        [[nodiscard]]
        constexpr auto
            operator&(E1 lhs, E2 rhs) -> impl::enable_operator_t<impl::bitmask_operation_and_result_t<E1, E2>> {
            using result_t = typename impl::enable_operator_t<impl::bitmask_operation_and_result_t<E1, E2>>;

            return result_t{
                static_cast<impl::get_enum_t<E1>>(impl::get_underlying(lhs) & impl::get_underlying(rhs))
            };
        }


        template<typename E1, typename E2>
        [[nodiscard]]
        constexpr auto
            operator^(E1 lhs, E2 rhs) -> impl::enable_operator_t<impl::bitmask_operation_xor_result_t<E1, E2>> {
            using result_t = typename impl::enable_operator_t<impl::bitmask_operation_xor_result_t<E1, E2>>;

            return result_t{
                static_cast<impl::get_enum_t<E1>>(impl::get_underlying(lhs) ^ impl::get_underlying(rhs))
            };
        }


        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bitmask<E>>
            operator~(E value) {
            return { static_cast<E>(
                ~impl::get_underlying(value)
                ) };
        }

        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, E>
            operator~(bitmask<E> value) {
            return { static_cast<E>(
                ~impl::get_underlying(value)
                ) };
        }


        template<typename E1, typename E2>
        constexpr auto
            operator|=(E1& lhs, E2 rhs) -> std::enable_if_t<std::is_same_v<E1, impl::bitmask_operation_or_result_t<E1, E2>>, E1&> {
            lhs = lhs | rhs;
            return lhs;
        }


        template<typename E1, typename E2>
        constexpr auto
            operator&=(E1& lhs, E2 rhs) -> std::enable_if_t<std::is_same_v<E1, impl::bitmask_operation_and_result_t<E1, E2>>, E1&> {
            lhs = lhs & rhs;
            return lhs;
        }

        template<typename E1, typename E2>
        constexpr auto
            operator^=(E1& lhs, E2 rhs) -> std::enable_if_t<std::is_same_v<E1, impl::bitmask_operation_xor_result_t<E1, E2>>, E1&> {
            lhs = lhs ^ rhs;
            return lhs;
        }

        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bool>
            operator!(E e) {
            return !impl::get_underlying(e);
        }

        // test for == 0 / != 0
        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bool>
            operator==(E value, std::nullptr_t) {
            return impl::get_underlying(value) == 0;
        }


        
        template<typename E1, typename E2>
        [[nodiscard]]
        constexpr auto
        operator&&(E1 lhs, E2 rhs) ->std::enable_if_t< enable_operators<typename impl::bitmask_operation_and_result_t<E1, E2>>::value, bool> {
            return impl::get_underlying(lhs) & impl::get_underlying(rhs);
        }
        
        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bool>
        operator||(E lhs, E rhs) {
            return impl::get_underlying(lhs) | impl::get_underlying(rhs);
        }




        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bool>
            any(E e) {
            return impl::get_underlying(e) != 0;
        }

        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, bool>
            none(E e) {
            return !e;
        }


        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, E>
            make_null(E) {
            return static_cast<E>(0);
        }


        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, E>
            make_if(E f, bool set) {
            return set ? f : static_cast<E>(0);
        }


        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, E>
            modify(E value, E modification, bool set) {
            return set ? (value | modification) : (value & ~modification);
        }

        template<typename E>
        [[nodiscard]]
        constexpr std::enable_if_t<enable_operators<E>::value, void>
            modify_inplace(E& value, E modification, bool set) {
            value = set ? (value | modification) : (value & ~modification);
        }




        // helper to get a value with n-th (zero-indexed) bit set
        [[nodiscard]]
        inline constexpr auto nth_bit(unsigned int n) { return 1 << n; }

    }
}


using boost::bitwise_operators::operator |;
using boost::bitwise_operators::operator &;
using boost::bitwise_operators::operator ^;
using boost::bitwise_operators::operator ~;
using boost::bitwise_operators::operator |=;
using boost::bitwise_operators::operator &=;
using boost::bitwise_operators::operator ^=;
using boost::bitwise_operators::operator !;

#endif  // BITWISE_OPERATORS_HPP_INCLUDED
