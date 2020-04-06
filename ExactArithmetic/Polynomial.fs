module ExactArithmetic.Polynomial

open System
open System.Linq
open ExactArithmetic.Rational

(* Polynomial([|a_0 ... a_n|]) = \sum_{i=0}^n a_i X^i *)
type Polynomial(coefficients: Rational []) =
    static member Constant r = Polynomial.withoutLeadingZero ([| r |])

    static member X =
        Polynomial
            ([| Rational(0, 1)
                Rational(1, 1) |])

    member this.Coefficients = coefficients
    member private this.indices = seq { 0 .. this.Degree }
    member this.Degree = coefficients.Length - 1

    member private this.leadingCoefficient =
        if coefficients.Length > 0 then coefficients.[coefficients.Length - 1]
        else Rational(0, 1)

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

    static member Remainder (P: Polynomial, Q: Polynomial) =
        let rec remainder_rec (P: Polynomial) (Q: Polynomial) =
            if Q.Degree.Equals(0) then raise (DivideByZeroException())
            if P.Degree < Q.Degree then
                P
            else
                let factor = (Rational.MultiplicativeInverse Q.leadingCoefficient) * P.leadingCoefficient
                let d = P.Degree - Q.Degree
                remainder_rec (P - factor * (Polynomial.Power (Polynomial.X, d)) * Q) Q
        remainder_rec P Q

    static member (%) (P: Polynomial, Q: Polynomial) = Polynomial.Remainder(P,Q)
    
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
