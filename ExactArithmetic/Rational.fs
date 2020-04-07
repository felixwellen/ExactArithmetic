module ExactArithmetic.Rational

open System.Numerics

(*

Rational numbers represented by two integers:

p    up
- = ----
q   down

*)

type Rational(up_unreduced: BigInteger, down_unreduced: BigInteger) =
  (* the 'let's here are constructor code *)
  let rec GreatestCommonDivisor a (b: BigInteger) =
    if b.IsZero then a else
      GreatestCommonDivisor b (a % b)

  let up_reduced, down_reduced = 
    if down_unreduced.IsZero then raise (System.DivideByZeroException())
    let g = GreatestCommonDivisor up_unreduced down_unreduced
    (up_unreduced / g, down_unreduced / g)
    
  (* make it possible to write 'Rationl(1,2)' instead of 'Rational(BigInteger(1), BigInteger(2))' *)
  new(i: int, j: int) = Rational(BigInteger(i), BigInteger(j))
  new(i: int) = Rational(BigInteger(i), BigInteger(1))  
  member private this.up = up_reduced
  member private this.down = down_reduced

  override this.ToString() =
    if this.down.IsOne then this.up.ToString() else sprintf "%A/%A" (this.up.ToString()) (this.down.ToString())

  override l.Equals r =
    match r with
    | :? Rational as r -> (l.up * r.down).Equals(l.down * r.up)
    | _ -> false

  override this.GetHashCode () = (this.up.GetHashCode()) + 17 * (this.down.GetHashCode())
  member this.IsZero = this.up.CompareTo(BigInteger(0)).Equals(0)
  member this.IsOne = this.up.CompareTo(BigInteger(1)).Equals(0) && this.down.CompareTo(BigInteger(1)).Equals(0)

  static member Zero = Rational(BigInteger(0),BigInteger(1))
  static member One = Rational(BigInteger(1),BigInteger(1))
  static member MultiplicativeInverse (r: Rational) =
      if r = Rational(BigInteger(0),BigInteger(1))
      then invalidArg "r" "0"
      else Rational(r.down, r.up)
      
  static member (+) (l: Rational, r: Rational) =
    Rational(l.up * r.down + r.up * l.down,
             l.down * r.down)

  static member (-) (l: Rational, r: Rational) =
    Rational(l.up * r.down - r.up * l.down,
             l.down * r.down)

  static member (*) (l: Rational, r: Rational) =
    Rational(l.up * r.up,
             l.down * r.down)

  static member (/) (l: Rational, r: Rational) =
    Rational(l.up * r.down,
             l.down * r.up)

