# bitwise_operators
type-safe bitwise-operations on enums in C++

    enum class flags_t {
        a = 0x1,
        b = 0x2,
        c = 0x4,
    };
    template<>
    struct enable_bitwise_operators<flags_t> : std::true_type {};
    void foo() {
        auto ab = flags_t::a | flags_t::b;
        auto bc = flags_t::b | flags_t::c;
        auto ab_and_bc = ab & bc;
        auto not_a = ~flags_t::a;   // a mask
        // test with boolean result
        if (ab_and_bc && not_a) {
            // ...
        }
    }
