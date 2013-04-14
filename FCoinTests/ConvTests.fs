module ConvTests

open NUnit.Framework

let (=?=) x y = Assert.True((x = y))

module ConvHexTests = 
  open Conv.Hex

  let [<Test>] fromBytes () =
    "303132333435363738" =?= fromBytes "012345678"B

  let [<Test>] toBytes () =
    "012345678"B =?= toBytes "303132333435363738"

  let [<Test>] fromBigInt () =
    "00" =?= fromBigInt 0I
    "01" =?= fromBigInt 1I
    "10" =?= fromBigInt 16I
    "FF" =?= fromBigInt 255I
    "0100" =?= fromBigInt 256I
    "FFFF" =?= fromBigInt 65535I

  let [<Test>] toBigInt ()=
    0I =?= toBigInt "00"
    0I =?= toBigInt "0"
    1I =?= toBigInt "01"
    1I =?= toBigInt "1"
    1I =?= toBigInt "001"
    16I =?= toBigInt "10"
    16I =?= toBigInt "010"
    255I =?= toBigInt "FF"
    255I =?= toBigInt "000FF"
    256I =?= toBigInt "0100"
    256I =?= toBigInt "100"
    65535I =?= toBigInt "FFFF"
    256I =?= toBigInt "01 00"
    65535I =?= toBigInt "FF FF"

module ConvBytesTests = 
  let [<Test>] pad () = 
    "\x00\x00\x00\x00ABCD"B =?= ("ABCD"B |> Conv.Bytes.pad 8)

module ConvUnsignedBigTests =
  open Conv.UnsignedBig

  let [<Test>] fromBytes () =
    0I =?= fromBytes "\x00"B
    255I =?= fromBytes "\xFF"B
    256I =?= fromBytes "\x01\x00"B
    65535I =?= fromBytes "\xFF\xFF"B
    65536I =?= fromBytes "\x01\x00\x00"B

  let [<Test>] toBytes () =
    "\x00"B =?= toBytes 0I
    "\xFF"B =?= toBytes 255I
    "\x01\x00"B =?= toBytes 256I
    "\xFF\xFF"B =?= toBytes 65535I
    "\x01\x00\x00"B =?= toBytes 65536I


module ConvBase58Tests =
  open Conv.Base58

  let [<Test>] verifyBase58Check () =
    let examples = [
        0uy, "Bitcoin pubkey hash", "12CPLrAUPvhVwjZqBgww3sLdEg4Z888R1j"
        5uy, "Bitcoin script hash", "3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX"
        48uy, "Litecoin pubkey hash", "LhK2kQwiaAvhjWY799cZvMyYwnQAcxkarr"
        52uy, "Namecoin pubkey hash", "NATX6zEUNfxfvgVwz8qVnnw3hLhhYXhgQn"
        95uy, "Fairbrix pubkey hash", "fF6o8LeDAfswEpMbCW8BqaqmzMWS7TGgew"
        97uy, "GeistGeld pubkey hash", "gQ8YScyiMUTart6kUJpzhjPzAKfiYAwooc"
        105uy, "i0coin pubkey hash", "jWmCr5cKeQjV4iyfUyipfLGwVML8MvXhF2"
        111uy, "Bitcoin testnet pubkey hash", "mkJ7Bf5chdfw61d1m7gnDVAQV3EQQAb8iz"
        125uy, "Solidcoin pubkey hash", "sXNaMoYBocjcQJRLK53dkaQ5mWuKfvHB9f"
        127uy, "Tenebrix pubkey hash", "tUK2EQTMF6cN6vuNEfJtVf1BMqarvEZJBL"
        128uy, "Bitcoin Private key (for uncompressed pubkey)", "5Htn3FzuH3b1X5VF2zLTsAQzBcyzkZNJsa2egXN8ZFJTCqQm3Rq"
        128uy, "Bitcoin Private key (for compressed pubkey)", "L1aW4aubDFB7yfras2S1mN3bqg9nwySY8nkoLmJebSLD5BWv3ENZ"
        138uy, "ixcoin pubkey hash", "xoKDFH4uWpyzxUcCC5jCLFujRKayv3HHcV"
        239uy, "Testnet Private key (for uncompressed pubkey)", "91eWjgRmucdtYHpMdsHbn9h8UU8hdoMNSKj8p3QAj6VTLyBnjj6"
        239uy, "Testnet Private key (for compressed pubkey)", "cNJFgo1driFnPcBdBX8BrJrpxchBWXwXCvNH5SoSkdcF6JXXwHMm"
        ]
    for expectedVersion, description, encodedString in examples do
        match verify encodedString with
        | Some (version, payload) ->
            Assert.True((version=expectedVersion))
        | None ->
            Assert.Fail("failed verifying base58 string ({0}) {1} ", description, encodedString)  

  let [<Test>] toBase58check () = 
    let shouldbe = "5K5zGnFRJAD3viw93hPQLq5Fa3xsLFGYC9yoGMcmVdS1CEaKYs7" 
    let is = "a7593394a809fe36b10fb3203480dd789fd12e771b07e3df50b37080714f1d2e" |> Conv.Hex.toBytes |> toBase58check 0x80uy
    shouldbe =?= is
    

module ConvBitcoinTests =
  open Conv.Bitcoin
  
  let [<Test>] toWalletImportFormat () =
    let priv = "a7593394a809fe36b10fb3203480dd789fd12e771b07e3df50b37080714f1d2e" |> Conv.Hex.toBigInt
    "5K5zGnFRJAD3viw93hPQLq5Fa3xsLFGYC9yoGMcmVdS1CEaKYs7" =?=  toWalletImportFormat (false, priv)

  let [<Test>] toAddressFormat () =
    "1G3zNdwLDQ5Bgcd7axEmJ4LFEJkPPWpEaN" =?= ("5J8LsrnPk9SqwntTvawwGbaSMxpjkVtVUVZCHoNwiG2TcZ2Ca4q" |> fromWalletImportFormat |> EcDsa.secp256k1.getPubKey |> pubToAddress)
