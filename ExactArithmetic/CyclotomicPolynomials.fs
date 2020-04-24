module ExactArithmetic.CyclotomicPolynomial

open System.Collections.Generic
open ExactArithmetic
open ExactArithmetic.Integer
open ExactArithmetic.Polynomial

type CyclotomicPolynomials() =
    let X = Polynomial.X
    let One = Polynomial.One
    let startKnowing s =
          let dictionary = Dictionary<int64,Polynomial>()
          Seq.iter (fun (k,v) -> dictionary.Add(k, v)) s
          dictionary
          
    let known = startKnowing [1L, X - One
                              2L, X + One]
    
    (*
        Computation of the n-th cyclotomic polynomial following
        
        https://en.wikipedia.org/wiki/Cyclotomic_polynomial
    
    *)
    member this.Phi(n: int64) =
            let isKnown, maybePolynomial = known.TryGetValue (n)
            if isKnown
            then maybePolynomial
            else
                let polynomial =
                    let divisors = Integer(n).NontrivialDivisors()
                    List.foldBack (fun d (P: Polynomial) -> let Q,_ = Polynomial.DivisionWithRemainder (P, this.Phi((int64)d)) in Q)
                                  divisors
                                  ((Polynomial.Power(X,n) - Polynomial.One) / (X - One))
                in
                known.Add(n, polynomial)
                polynomial
                              
