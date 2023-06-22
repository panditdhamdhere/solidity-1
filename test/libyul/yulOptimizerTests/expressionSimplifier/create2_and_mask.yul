{
    // This is not fully simplified on purpose because we
    // need another split step in between. The full simplification
    // is tested in the fullSuite.
    let a := and(create2(0, 0, 0x20, 0), 0xffffffffffffffffffffffffffffffffffffffff)
    let b := and(0xffffffffffffffffffffffffffffffffffffffff, create2(0, 0, 0x20, 0))
    sstore(a, b)
}
// ====
// EVMVersion: >=shanghai
// ----
// step: expressionSimplifier
//
// {
//     {
//         let _1 := 0xffffffffffffffffffffffffffffffffffffffff
//         let _2 := 0x20
//         let a := and(create2(0, 0, _2, 0), _1)
//         sstore(a, and(_1, create2(0, 0, _2, 0)))
//     }
// }
