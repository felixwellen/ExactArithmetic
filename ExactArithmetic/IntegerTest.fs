module ExactArithmetic.IntegerTest

open System
open System.Numerics
open Xunit
open ExactArithmetic.IntegerWithDivisibilityInformation

[<Fact>]
let ``rounded square root is the exact square root for squares`` () =
    let someSquare = Integer(91L*91L) in
    Assert.Equal(91L, someSquare.RoundedSquareRoot())
    
[<Fact>]
let ``small divisors contain the root of a square`` () =
    let someSquare = Integer(191L*191L) in
    Assert.Contains(191L, Seq.toList(someSquare.SmallDivisors()))
    
[<Fact>]
let ``square roots are rounded away from zero`` () =
    let someSquare = Integer(191L*191L) in
    Assert.Contains(191L, Seq.toList(someSquare.SmallDivisors()))
    
[<Fact>]
let ``a prime is its own smallest prime divisor`` () =
    let somePrime = BigInteger(123457L)
    Assert.Equal(somePrime, Integer(somePrime).SmallestPrimeDivisor())
    
[<Fact>]
let ``a prime has an empty sequence of non-trivial divisors`` () =
    let prime = BigInteger(123457L)
    Assert.Empty(Integer(prime).NontrivialPrimeDivisorsWihtMultiplicities())

[<Fact>]
let ``products of primes have the primes as non-trivial prime divisors`` () =
    let prime1 = BigInteger(123457L)
    let prime2 = BigInteger(97L)
    
    let divisors = Integer(prime1 * prime2).NontrivialPrimeDivisorsWihtMultiplicities()
    
    Assert.Contains(prime1, divisors)
    Assert.Contains(prime2, divisors)
    Assert.Equal(2, divisors.Length)
    
[<Fact>]
let ``products of primes have the primes as non-trivial divisors`` () =
    let prime1 = BigInteger(123457L)
    let prime2 = BigInteger(97L)
    
    let divisors = Integer(prime1 * prime2).NontrivialDivisors()
    
    Assert.Contains(prime1, divisors)
    Assert.Contains(prime2, divisors)
    Assert.Equal(2, divisors.Length)

[<Fact>]
let ``prime powers times a prime has the expected sequence of non-trivial prime divisors`` () =
    let prime1 = BigInteger(123457L)
    let prime2 = BigInteger(97L)
    
    let number = prime2 * prime2 * prime1
    let divisors = Integer(number).NontrivialPrimeDivisorsWihtMultiplicities()
    
    Assert.Contains(prime1, divisors)
    Assert.Contains(prime2, divisors)
    Assert.Contains(prime2, divisors)
    Assert.Equal(3, divisors.Length)

[<Fact>]
let ``non-trivial divisors are the ones we learnt in school`` () =
    let someNumber = Integer(12L)
    
    let divisors = someNumber.NontrivialDivisors()
    
    Assert.Contains(BigInteger(2), divisors)
    Assert.Contains(BigInteger(3), divisors)
    Assert.Contains(BigInteger(4), divisors)
    Assert.Contains(BigInteger(6), divisors)
    Assert.Equal(4, divisors.Length)
