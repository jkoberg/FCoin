module Vanity

let genPrivkey = 
  let random = Radix.BigEndian.toBigInt (Random.bytes 256)
  let pub = EcDsa.secp256k1.multiply random EcDsa.secp256k1.G
  Base58.toBase58check 0uy [||]


  