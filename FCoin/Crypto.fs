
module Crypto

open System.Security.Cryptography


let Sha256 = HashAlgorithm.Create("SHA256")

let sha256 (input:byte[]) = Sha256.ComputeHash(input)


let RipeMD160 = HashAlgorithm.Create("RIPEMD160")

let ripemd160 (input:byte[]) = RipeMD160.ComputeHash(input)


let Rng = RNGCryptoServiceProvider.Create()

let randBytes count =
  let mutable out:byte[] = Array.zeroCreate count
  do Rng.GetBytes(out)
  out

let randBits bitcount =
  let bytecount = int (ceil (float bitcount)/8.)
  let le = randBytes bytecount |> Array.append "\x00"B
  bigint le