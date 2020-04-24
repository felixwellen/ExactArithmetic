module ExactArithmetic.Tests

open Xunit
open ExactArithmetic.Rational

[<Fact>]
let ``addition of rationals is correct in a small example``() =
    let p = Rational(2, 3)
    let q = Rational(-4, 7)
    let expected_sum = Rational(2, 21)
    Assert.Equal(expected_sum, p + q)
    
[<Fact>]
let ``reduced fractions have the expected equality of rationals``() =
    let p = Rational.Reduced(2, 3)
    let q = Rational.Reduced(4, 6)
    Assert.Equal(p, q)

[<Fact>]
let ``addition is associative``() =
    let p = Rational(2, 3)
    let q = Rational(-4, 7)
    let r = Rational(123, 17)
    Assert.Equal(p + (q + r), (p + q) + r)

[<Fact>]
let ``addition is commutative``() =
    let q = Rational(-4, 7)
    let r = Rational(123, 17)
    Assert.Equal(q + r , r + q)

[<Fact>]
let ``addition of rationals is invertible``() =
    let p = Rational(1, 2)
    Assert.Equal(Rational.Zero, p - p)

[<Fact>]
let ``mulitplication is associative``() =
    let p = Rational(2, 3)
    let q = Rational(-4, 7)
    let r = Rational(123, 17)
    Assert.Equal(p * (q * r), (p * q) * r)

[<Fact>]
let ``mulitplication distributes over addition``() =
    let p = Rational(2, 3)
    let q = Rational(-4, 7)
    let r = Rational(123, 17)
    Assert.Equal(p * (q + r), (p * q) + (p * r))


