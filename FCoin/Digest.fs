module Digest

let Sha256 = System.Security.Cryptography.HashAlgorithm.Create("SHA256")

let RipeMD160 = System.Security.Cryptography.HashAlgorithm.Create("RIPEMD160")

let sha256 (input:byte[]) = Sha256.ComputeHash(input)

let ripemd160 (input:byte[]) = RipeMD160.ComputeHash(input)

let shaRipePair input =
    let firstRound = sha256 input
    (sha256 firstRound, ripemd160 firstRound)

