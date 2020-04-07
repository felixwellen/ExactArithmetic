module ExactArithmetic.Program

open ExactArithmetic.Rational
open ExactArithmetic.Polynomial

[<EntryPoint>]
let main argv =
    printf "Only unit tests so far...\n"
    let P = Polynomial([|Rational(1,2);Rational.Zero;Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,5);Rational(-3,7)|]) in
    let result, remainder = Polynomial.DivisionWithRemainder (P,Q) in
    printf "%A" (P.ToString())
    printf "%A" ((result * Q + remainder).ToString())
    0 
