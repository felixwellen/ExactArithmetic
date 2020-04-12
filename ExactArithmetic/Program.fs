module ExactArithmetic.Program

open System.Numerics
open ExactArithmetic.Integer
open ExactArithmetic.Polynomial

[<EntryPoint>]
let main argv =
    printfn "Only unit tests so far...\n"
    Seq.iter (fun n -> printfn "Phi(%d): %A" n (Polynomial.Phi(n).ToString())) (seq { 1L .. 360L })
    0
