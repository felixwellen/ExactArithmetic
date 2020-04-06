module ExactArithmetic.Rational

open System.Numerics

(*

Rational numbers represented by two integers:

p    up
- = ----
q   down

*)

type Rational(up: BigInteger, down: BigInteger) =
  (* make it possible to write 'Rationl(1,2)' instead of 'Rational(BigInteger(1), BigInteger(2))' *)
  new(i: int, j: int) = Rational(BigInteger(i), BigInteger(j))

  member private this.up = up
  member private this.down = down

  override this.ToString() =
    if down.IsOne then up.ToString() else sprintf "%A/%A" (up.ToString()) (down.ToString())

  override l.Equals r =
    match r with
    | :? Rational as r -> (l.up * r.down).Equals(l.down * r.up)
    | _ -> false

  override this.GetHashCode () = (up.GetHashCode()) + 17 * (down.GetHashCode())
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

