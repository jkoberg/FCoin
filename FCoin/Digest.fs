
module Digest

let Sha256 = System.Security.Cryptography.HashAlgorithm.Create("SHA256")

let sha256 (input:byte[]) = Sha256.ComputeHash(input)


let RipeMD160 = System.Security.Cryptography.HashAlgorithm.Create("RIPEMD160")

let ripemd160 (input:byte[]) = RipeMD160.ComputeHash(input)

