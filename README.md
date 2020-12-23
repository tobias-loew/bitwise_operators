# bitwise_operators
type-safe bitwise-operations on enums in C++

    enum class flags_t {
        a = 0x1,
        b = 0x2,
        c = 0x4,
    };
    enum class flags2_t {
        e = lunaticpp::bitwise_operators::nth_bit(0), // == 0x1
        f = lunaticpp::bitwise_operators::nth_bit(1), // == 0x2
        g = lunaticpp::bitwise_operators::nth_bit(2), // == 0x4
    };
    template<>
    struct lunaticpp::bitwise_operators::enable_operators<flags_t> : std::true_type {};
    template<>
    struct lunaticpp::bitwise_operators::enable_operators<flags2_t> : std::true_type {};
    void foo() {
        auto ab = flags_t::a | flags_t::b;  // type of ab is flags_t
        auto bc = flags_t::b | flags_t::c;  // type of bc is flags_t
        auto ab_and_bc = ab & bc;           // type of ab_and_bc is flags_t
        auto not_a = ~flags_t::a;           // type of not_a is lunaticpp::bitwise_operators::bitmask<flags_t>
        auto not_not_a = ~not_a;            // type of not_not_a is flags_t
        // auto ae = flags_t::a | flags2_t::e;  // compilation error (different enumerations)
        // test with boolean result
        if (ab_and_bc && not_a) {
            // ...
        }
    }


# The benefits of bitwise_operators
With bitwise_operators you can non-intrusively enably bitwise operations on scoped and unscoped flag-like enumerations.
- The bitwise-operations &, &=, |, |=, ^ and ^= when applied to an enabled enumeration all return the type of the enumeration again.
- The negation-operation operator~(E e) returns as type bitmask<E> tp account for the fact that the negation of a set of flags is in general *not* a set flags but has also all "unused" bits sets (i.e. the operations ~, &, | are also a boolean algebra on the the set {E, bitmask<E>}). The library takes this into account and e.g. only prevents modifying operatiors &=, |= and ^= for incompatible types. E.g. operator |=(E& e1, bitmaks<E> e2) is not allowed as it would turn e1 from a combinations of flags into a bitmask.
