module ExactArithmetic.NumberFieldTest

open Xunit
open ExactArithmetic.Rational
open ExactArithmetic.Polynomial
open ExactArithmetic.NumberField

let TwoAsPolynomial = Polynomial([|Rational(2,1)|])
let ModulusForSquareRootOfTwo = Polynomial.Power(Polynomial.X,2) - TwoAsPolynomial
let E = NumberField(ModulusForSquareRootOfTwo)   (* A number field where 2 has a square root *)
let TwoAsNumberFieldElement = NumberFieldElement(E, TwoAsPolynomial)

[<Fact>]
let ``2=1+1 holds in the number field``() =
    Assert.Equal(TwoAsNumberFieldElement, E.One + E.One)

[<Fact>]
let ``the abstract solution is not zero``() =
    let e = E.Solution in  (* e is a solution of the equation 'X^2-2=0' *)
    Assert.False(E.Zero.Equals(e))

[<Fact>]
let ``the abstract solution of an equation as element of a number field is really a solution``() =
    let e = E.Solution in  (* e is a solution of the equation 'X^2-2=0' *)
    Assert.Equal(E.Zero, e * e - TwoAsNumberFieldElement)
