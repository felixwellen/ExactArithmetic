module ExactArithmetic.Rational

open System.Numerics

(*

Rational numbers represented by two integers:

p    up
- = ----
q   down

*)

[<Struct>]
type Rational(up: BigInteger, down: BigInteger) =
    static member Reduced(i: int, j: int) = Rational.Reduced(BigInteger(i), BigInteger(j))
    static member Reduced(up_unreduced: BigInteger, down_unreduced: BigInteger) =
        let rec GreatestCommonDivisor a (b: BigInteger) =
            if b.IsZero then a
            else GreatestCommonDivisor b (a % b)

        let up_reduced, down_reduced =
            if down_unreduced.IsZero then raise (System.DivideByZeroException())
            let g = GreatestCommonDivisor up_unreduced down_unreduced
            (up_unreduced / g, down_unreduced / g)
        Rational(up_reduced, down_reduced)
        
    (* make it possible to write 'Rationl(1,2)' instead of 'Rational(BigInteger(1), BigInteger(2))' *)
    new(i: int, j: int) = Rational(BigInteger(i), BigInteger(j))
    new(i: int) = Rational(BigInteger(i), BigInteger(1))
    
   
    override this.ToString() =
        if down.IsOne then up.ToString()
        else sprintf "%A/%A" (up.ToString()) (down.ToString())
    member this.IsZero = up.CompareTo(BigInteger(0)).Equals(0)
    member this.IsOne = up.CompareTo(BigInteger(1)).Equals(0) && down.CompareTo(BigInteger(1)).Equals(0)

    member this.Down = down 
    member this.Up = up 
    static member Zero = Rational(BigInteger(0), BigInteger(1))
    static member One = Rational(BigInteger(1), BigInteger(1))

    static member MultiplicativeInverse(r: Rational) =
        if r = Rational(BigInteger(0), BigInteger(1))
        then invalidArg "r" "0"
        else Rational.Reduced(r.Down, r.Up)

    static member (+) (l: Rational, r: Rational) = Rational.Reduced(l.Up * r.Down + r.Up * l.Down, l.Down * r.Down)

    static member (-) (l: Rational, r: Rational) = Rational.Reduced(l.Up * r.Down - r.Up * l.Down, l.Down * r.Down)

    static member (*) (l: Rational, r: Rational) = Rational.Reduced(l.Up * r.Up, l.Down * r.Down)

    static member (/) (l: Rational, r: Rational) = Rational.Reduced(l.Up * r.Down, l.Down * r.Up)
