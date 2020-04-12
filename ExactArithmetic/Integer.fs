module ExactArithmetic.Integer

open System
open System
open System.Collections
open System.Numerics

(* Extends the Integers given by 'BigInteger' by some operations *)

type Integer(n : BigInteger) =
    new(i: int64) = Integer(BigInteger(i))
    member this.SquareRoot () = Math.Exp(BigInteger.Log(n) / 2.0)
    member this.RoundedSquareRoot () = int64(Math.Round(this.SquareRoot (), MidpointRounding.AwayFromZero))
    
    member this.AsBigInteger = n
    
    member private this.CandidatesForDivisors () = seq { 2L .. (this.RoundedSquareRoot ()) } 
    member this.SmallDivisors () =
        Seq.filter (fun (k: int64) -> this.IsDividedBy(k))
                   (this.CandidatesForDivisors ())
                   
                   
    member private this.IsDividedBy (k: int64) = n % BigInteger(k) = BigInteger(0)
    member this.SmallestPrimeDivisor () =
        let rec smallest_prime_divisor candidates =
            if Seq.isEmpty candidates then n
            else
                let candidate = Seq.head candidates
                let remainingCandidates = Seq.tail candidates
                if (this.IsDividedBy(candidate)) then BigInteger(candidate)
                else smallest_prime_divisor (Seq.filter (fun (l: int64) -> not(l % candidate = 0L)) remainingCandidates)
        smallest_prime_divisor (this.CandidatesForDivisors ())
        
    member this.NontrivialPrimeDivisors () =
        let rec nontrivial_divisors (N: BigInteger) = 
            if N.IsOne then List.empty
            else 
                let smallestDivisor = Integer(N).SmallestPrimeDivisor () in
                if smallestDivisor < n
                then List.Cons (smallestDivisor, nontrivial_divisors (N / smallestDivisor))   
                else List.empty
        nontrivial_divisors (this.AsBigInteger)

    member this.NontrivialDivisors () =
        let primeDivisors = this.NontrivialPrimeDivisors ()
        let numberOfPrimeDivisors = primeDivisors.Length
        let nontrivial (subsequence: BigInteger list) = not(subsequence.Length = 0) && not(subsequence.Length = numberOfPrimeDivisors)
        List.distinct (List.map (fun primeDivisorSelection -> List.foldBack (*) primeDivisorSelection (BigInteger(1)))
                                (List.filter nontrivial (Integer.AllNontrivialSubsequences primeDivisors)))
                
    static member private AllNontrivialSubsequences (list) =
        match list.Length with
        | 0 -> List.Cons (List.empty, List.empty)
        | _ ->
            let first = list.Head
            let subsequencesForTail = Integer.AllNontrivialSubsequences (list.Tail)
            let finalList = List.append (List.map (fun l -> List.Cons (first, l)) subsequencesForTail) 
                                        subsequencesForTail
            finalList                                        