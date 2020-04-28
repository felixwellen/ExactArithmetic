module ExactArithmetic.CyclotomicPolynomialsTest

open System
open ExactArithmetic.CyclotomicPolynomial
open Xunit
open ExactArithmetic.Rational
open ExactArithmetic.Polynomial

[<Fact>]
let ``phi is correct``() =
    let X = Polynomial.X
    let cyclotomics = CyclotomicPolynomials()
    Assert.Equal(X.Power(6) - X.Power(5) + X.Power(4) - X.Power(3) + X.Power(2) - X + Polynomial.One,  cyclotomics.Phi(14L))
    
