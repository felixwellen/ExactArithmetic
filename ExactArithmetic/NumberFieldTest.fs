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
let ``the abstract solution is a solution of the given equation``() =
    let e = E.Solution in  (* e is a solution of the equation 'X^2-2=0' *)
    Assert.Equal(E.Zero, e * e - TwoAsNumberFieldElement)

[<Fact>]
let ``every non-zero element of E has a multiplicative inverse``() =
    let someElement = E.Solution + TwoAsNumberFieldElement
    Assert.Equal (E.One, someElement * someElement.MultiplicativeInverse())

[<Fact>]
let ``a cyclotomic numberfield has the correct degree for a prime`` () =
    let someCyclotomicNumberField = NumberField.Cyclotomic(17L)
    Assert.Equal(16, someCyclotomicNumberField.Degree)    

[<Fact>]
let ``in a cyclotomic numberfield, the roots of unity add up to 0`` () =
    let someCyclotomicNumberField = NumberField.Cyclotomic(17L)
    let E = someCyclotomicNumberField
    let rootsOfUnity = Seq.map (fun k -> NumberFieldElement.Power(E.Solution, k)) (seq { 1L .. 17L })
    Assert.Equal(E.Zero, Seq.reduce (+) rootsOfUnity)