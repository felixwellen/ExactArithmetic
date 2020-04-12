module ExactArithmetic.NumberField

open System
open ExactArithmetic
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
    
    static member Cyclotomic(n: int64) =
        NumberField(Polynomial.Phi(n))

and NumberFieldElement(E: NumberField, coefficients: Rational []) =
    new(E: NumberField, P: Polynomial) = NumberFieldElement(E, P.Coefficients)
    member this.AsPolynomial: Polynomial = Polynomial(coefficients)
    member this.IsZero = this.AsPolynomial.IsZero
    member this.IsOne = this.AsPolynomial.IsOne
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
            
    member this.MultiplicativeInverse () =
        if this.IsZero then raise (DivideByZeroException()) 
        else let rec euclid_rec (old_r: Polynomial, r: Polynomial) = (* calculate linear combination of 1 of the successive remainders old_r,r *)
               let d, new_r = Polynomial.DivisionWithRemainder (old_r, r)
               if new_r.IsConstant then let f = Polynomial.Constant(Rational.MultiplicativeInverse(new_r.AsRational())) in
                                        f, Rational(-1) * d * f
               else let left, right = euclid_rec (r, new_r)   (* left * r + right * new_r = 1 *)
                    right, left - right * d
                    
             NumberFieldElement(this.NumberField,
                                if this.AsPolynomial.IsConstant then Polynomial.Constant(Rational.MultiplicativeInverse(this.AsPolynomial.AsRational()))
                                else let _, inverse = euclid_rec (this.NumberField.Modulus, this.AsPolynomial)
                                     inverse)
                  

    static member Power (x: NumberFieldElement, n: int64) =
        NumberFieldElement(x.NumberField, Polynomial.Power(x.AsPolynomial,n) % x.NumberField.Modulus)
        
    static member (/) (x: NumberFieldElement, y: NumberFieldElement) =
        NumberFieldElement.returnIfNumberFieldsMatch
            (x,y,
             NumberFieldElement(x.NumberField, (x.AsPolynomial * (y.MultiplicativeInverse().AsPolynomial))))
            
    override this.Equals a =
        match a with
        | :? NumberFieldElement as a -> if not(this.NumberField.Equals(a.NumberField)) then false
                                        else this.AsPolynomial.Equals(a.AsPolynomial)
        | _ -> false

    override this.GetHashCode () =
        this.AsPolynomial.GetHashCode()

    