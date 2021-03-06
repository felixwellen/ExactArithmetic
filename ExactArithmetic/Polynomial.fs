module ExactArithmetic.Polynomial

open System
open ExactArithmetic.Rational

(* Polynomial([|a_0 ... a_n|]) = \sum_{i=0}^n a_i X^i *)
type Polynomial(coefficients: Rational []) =
    interface IEquatable<Polynomial> with
            member this.Equals(that: Polynomial) =
                this.Equals(that)
                
    static member Constant r = Polynomial.withoutLeadingZero ([| r |])

    static member Zero = Polynomial.Constant Rational.Zero
    static member One = Polynomial.Constant Rational.One
    static member X =
        Polynomial
            ([| Rational(0, 1)
                Rational(1, 1) |])

    member this.Coefficients = coefficients
    member private this.indices = seq { 0 .. this.Degree }
    member this.Degree = coefficients.Length - 1
    member this.IsZero = this.Degree.Equals(-1)
    member this.IsOne = this.Equals(Polynomial.One)

    member this.LeadingCoefficient =
        if coefficients.Length > 0 then coefficients.[coefficients.Length - 1]
        else Rational(0, 1)
    member this.IsConstant = this.Degree.CompareTo(1) < 0
    member this.AsRational () = if not(this.IsConstant) then raise (ArithmeticException())
                                else this.LeadingCoefficient

                
    override this.Equals Q =
        match Q with
        | :? Polynomial as Q ->
            if this.Degree.Equals(Q.Degree) then
                Seq.forall (fun (r, q) -> r.Equals(q) ) (Seq.zip this.Coefficients Q.Coefficients)
            else false
        | _ -> false
        
    override this.GetHashCode () =
        if this.Degree.Equals(-1) then 0 
        else Seq.reduce (+) (Seq.map (fun coefficient -> coefficient.GetHashCode()) this.Coefficients)
    
    static member private withoutLeadingZero (coefficients: Rational []) =
        let index_of_leading_coefficient =
            match Seq.tryHead
                      (seq {
                          for i in Seq.rev (seq { 0 .. (coefficients.Length - 1) }) do
                              if not (coefficients.[i] = Rational(0, 1)) then yield i
                       }) with
            | Some i -> i
            | None -> -1
        Polynomial(Array.sub coefficients 0 (index_of_leading_coefficient + 1))


    static member (*) (P: Polynomial, Q: Polynomial) =
        let result_coefficients = Array.create (P.Degree + Q.Degree + 1) (Rational(0, 1))
        for i in P.indices do
            for j in Q.indices do
                result_coefficients.[i + j] <- result_coefficients.[i + j] + P.Coefficients.[i] * Q.Coefficients.[j]
        Polynomial(result_coefficients)

    static member (*) (r: Rational, Q: Polynomial) = (Polynomial.Constant r) * Q

    static member (+) (P: Polynomial, Q: Polynomial) =
        let copy_and_add_to (a1: Rational []) k (a2: Rational []) =
            let result = Array.copy a1
            for i in 0 .. k do
                result.[i] <- a1.[i] + a2.[i]
            result
        Polynomial.withoutLeadingZero
            (if (P.Degree > Q.Degree) then copy_and_add_to P.Coefficients (Q.Coefficients.Length - 1) Q.Coefficients
             else copy_and_add_to Q.Coefficients (P.Coefficients.Length - 1) P.Coefficients)

    static member (-) (P: Polynomial, Q: Polynomial) = P + (Rational(-1, 1) * Q)

    static member Power (P: Polynomial, n: int) =
        let rec power_rec P n =
            if n = 0 then
                Polynomial.Constant (Rational(1, 1))
            else if n = 1 then
                P
            else
                let tmp = (power_rec P (n / 2))
                if n % 2 = 0 then tmp * tmp
                else P * tmp * tmp
        power_rec P n

    static member Power (P: Polynomial, n: int64) =
        let rec power_rec P n =
            if n = 0L then
                Polynomial.Constant (Rational(1, 1))
            else if n = 1L then
                P
            else
                let tmp = (power_rec P (n / 2L))
                if n % 2L = 0L then tmp * tmp
                else P * tmp * tmp
        power_rec P n
        
    member this.Power(n: int64) =
        Polynomial.Power(this,n)
        
    member this.Power(n: int) =
        Polynomial.Power(this,n)
        
    static member Remainder (P: Polynomial, Q: Polynomial) =
        let rec remainder_rec (P: Polynomial) (Q: Polynomial) =
            if Q.Degree.CompareTo(0) < 0 then raise (DivideByZeroException())
            if P.Degree < Q.Degree then
                P
            else
                let factor = (Rational.MultiplicativeInverse Q.LeadingCoefficient) * P.LeadingCoefficient
                let d = P.Degree - Q.Degree
                remainder_rec (P - factor * (Polynomial.Power (Polynomial.X, d)) * Q) Q
        remainder_rec P Q
        
    static member (%) (P: Polynomial, Q: Polynomial) = Polynomial.Remainder(P,Q)

    static member DivisionWithRemainder (P: Polynomial, Q: Polynomial) =
        if Q.Degree.CompareTo(0) < 0 then raise (DivideByZeroException())
        let rec division_with_remainder (P: Polynomial, Q: Polynomial, old_division_result: Polynomial) =
            if P.Degree < Q.Degree then
                P, Q, old_division_result
            else
                let factor = (Rational.MultiplicativeInverse Q.LeadingCoefficient) * P.LeadingCoefficient
                let d = P.Degree - Q.Degree
                let division_result = factor * (Polynomial.Power (Polynomial.X, d))
                division_with_remainder (P - division_result * Q, Q, old_division_result + division_result)
        let remainder, _, division_result = division_with_remainder (P, Q, Polynomial.Zero) in
        division_result, remainder
    
    static member (/) (P: Polynomial, Q: Polynomial) =
        let Q,_ = Polynomial.DivisionWithRemainder (P, Q)
        Q
        
    override this.ToString() =
        if coefficients.Length = 0 then
            "0"
        else
            let d = coefficients.Length

            let print_summand ((d, c): int * Rational) =
                let c =
                    if not (d.Equals(0)) && c.IsOne then ""
                    else c.ToString()
                if (d > 1) then c + " X^" + d.ToString()
                else if (d = 1) then c + " X"
                else c

            let coefficients_with_degree = Seq.zip (seq { 0 .. d }) coefficients
            let non_zero_summands =
                Seq.filter (fun ((_, c): int * Rational) -> not (c.IsZero)) coefficients_with_degree
            let summands = Seq.map print_summand non_zero_summands
            Seq.reduce (fun s1 s2 -> s1 + " + " + s2) summands

    (*
        Finite segments of the geometric series, i.e.
        
        G(n) := X^(n-1) + ... + 1
        
        which has the property
        
        (X^n - 1) = (X - 1) G(n)
    *)
    static member GeometricSeries(n: int) =
        Polynomial(Array.replicate n Rational.One)
        