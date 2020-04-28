module ExactArithmetic.TestPolynomial

open System
open Xunit
open ExactArithmetic.Rational
open ExactArithmetic.Polynomial

[<Fact>]
let ``the polynomials 'constant 0' and 'constant 1' are different``() =
    let zero = Polynomial.Constant(Rational.Zero)
    let one = Polynomial.Constant(Rational.One)
    Assert.NotEqual(zero, one)


[<Fact>]
let ``equality also works for more complicated polynomials``() =
    let P = Polynomial([|Rational(1,2);Rational.Zero;Rational(2,4)|]) in
    let Q = Polynomial([|Rational(1,2);Rational.Zero;Rational(3,4)|]) in
    Assert.NotEqual(P, Q)

[<Fact>]
let ``addition of polynomials is correct in a small example``() =
    let P = Polynomial([|Rational(1,2);Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,2);Rational(-3,4)|]) in
    let expected_sum = Polynomial([|Rational(1,1)|])
    Assert.Equal(expected_sum, P + Q)
    
[<Fact>]
let ``mulitplication distributes over addition``() =
    let P = Polynomial([|Rational(1,2);Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,5);Rational(-3,7)|]) in
    let R = Polynomial([|Rational(-7,13);Rational.Zero;Rational.Zero;Rational(-3,4)|]) in
    Assert.Equal(P * (Q + R), (P * Q) + (P * R))
    
[<Fact>]
let ``the degree is correct``() =
    let P = Polynomial([|Rational(-7,13);Rational.Zero;Rational.Zero;Rational(-3,4)|]) in
    Assert.Equal(3, P.Degree)

[<Fact>]
let ``powers are repeated multiplications``() =
    let P = Polynomial([|Rational(-7,13);Rational.Zero;Rational.Zero;Rational(-3,4)|]) in
    Assert.Equal(P * P * P * P, Polynomial.Power(P, 4))
    
[<Fact>]
let ``a small enough polynomial can be added to a product and retrieved with 'Remainder'``() =
    let P = Polynomial([|Rational(1,2);Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,5);Rational(-3,7)|]) in
    let R = Polynomial([|Rational(-7,13);Rational.Zero;Rational.Zero;Rational(-3,4)|]) in
    let composedPolynomial = Q * R + P in (* P can be extracted again, since 'degree(R) > degree(P)' *)
    let remainder = Polynomial.Remainder(composedPolynomial, R) in
    Assert.Equal(P , remainder)
    
[<Fact>]
let ``there is no 'Remainder' of anything with respect to the zero-polynomial``() =
   let P = Polynomial([|Rational(1,2);Rational(3,4)|]) in
   Assert.Throws(typeof<DivideByZeroException>, fun () -> P % Polynomial.Zero |> ignore)
   
[<Fact>]
let ``(Q*P) % P is zero``() =
    let P = Polynomial([|Rational(1,2);Rational.Zero;Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,5);Rational(-3,7)|]) in
    Assert.Equal(Polynomial.Zero, (P*Q) % P)
    
[<Fact>]
let ``'DivisionWithRemainder' yields the expected decomposition``() =
    let P = Polynomial([|Rational(1,2);Rational.Zero;Rational(3,4)|]) in
    let Q = Polynomial([|Rational(1,5);Rational(-3,7)|]) in
    let result, remainder = Polynomial.DivisionWithRemainder (P,Q) in
    Assert.True(P.Equals(result * Q + remainder))
    
[<Fact>]
let ``Test Geometric Series`` () =
    let G = Polynomial.GeometricSeries(25)
    Assert.Equal(Polynomial.X.Power(25) - Polynomial.One, G * (Polynomial.X - Polynomial.One))