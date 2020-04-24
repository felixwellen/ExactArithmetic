module ExactArithmetic.Program

open System.Numerics
open ExactArithmetic.CyclotomicPolynomial
open ExactArithmetic.Integer
open ExactArithmetic.Polynomial

[<EntryPoint>]
let main argv =
    let cyclotomics = CyclotomicPolynomials()
    Seq.iter
        (fun n -> let P = cyclotomics.Phi(n) in
                  if (n % 7L = 0L) then printfn "Phi(%d): %A" n (P.ToString())
                  else ())
        (seq { 1L .. 75L })
    0
