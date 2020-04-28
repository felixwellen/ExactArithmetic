module ExactArithmetic.CyclotomicPolynomial

open System.Collections.Generic
open ExactArithmetic
open ExactArithmetic.IntegerWithDivisibilityInformation
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
                    if divisors.IsEmpty then Polynomial.GeometricSeries((int)n)
                    else 
                        let cyclotomicsForDivisors = Seq.map (fun d -> this.Phi((int64)d)) divisors
                        let D = Seq.reduce (*) cyclotomicsForDivisors
                        (Polynomial.GeometricSeries((int)n)) / D
                in
                known.Add(n, polynomial)
                polynomial
