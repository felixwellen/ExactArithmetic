module ExactArithmetic.NumberField

open ExactArithmetic
open System
open ExactArithmetic.Rational
open ExactArithmetic.Polynomial

(*
the modulus of a number field is assumed to be a non-constant
irreducible polynomial
*)
type NumberField(modulus: Polynomial) =
    member this.Modulus = modulus
    member this.Degree = modulus.Degree
    member this.Zero = NumberFieldElement(this, Polynomial.Constant(Rational(0,1)))
    member this.One = NumberFieldElement(this, Polynomial.Constant(Rational(1,1)))
    member this.From (r: Rational) = NumberFieldElement(this, Polynomial.Constant(r))
    member this.Solution = NumberFieldElement(this, Polynomial.X)



and NumberFieldElement(E: NumberField, coefficients: Rational []) =
    new(E: NumberField, P: Polynomial) = NumberFieldElement(E, P.Coefficients)
    member this.AsPolynomial: Polynomial = Polynomial(coefficients)
    member this.NumberField: NumberField = E
    static member private returnIfNumberFieldsMatch (x: NumberFieldElement, y: NumberFieldElement,
                                                     returnValue: NumberFieldElement): NumberFieldElement =
        if not (x.NumberField.Equals(y.NumberField)) then invalidArg "y.NumberField" "not x.NumberField"
        else returnValue
        
    static member (+) (x: NumberFieldElement, y: NumberFieldElement): NumberFieldElement =
        NumberFieldElement.returnIfNumberFieldsMatch (x, y, NumberFieldElement(x.NumberField, x.AsPolynomial + y.AsPolynomial))

    static member (-) (x: NumberFieldElement, y: NumberFieldElement): NumberFieldElement =
        NumberFieldElement.returnIfNumberFieldsMatch
           (x, y, NumberFieldElement(x.NumberField, x.AsPolynomial - y.AsPolynomial))
           
    static member (*) (x: NumberFieldElement, y: NumberFieldElement): NumberFieldElement =
        NumberFieldElement.returnIfNumberFieldsMatch
            (x, y, NumberFieldElement(x.NumberField, (x.AsPolynomial * y.AsPolynomial) % x.NumberField.Modulus))

    override this.Equals a =
        match a with
        | :? NumberFieldElement as a -> if not(this.NumberField.Equals(a.NumberField)) then false
                                        else this.AsPolynomial.Equals(a.AsPolynomial)
        | _ -> false

    override this.GetHashCode () =
        this.AsPolynomial.GetHashCode()
