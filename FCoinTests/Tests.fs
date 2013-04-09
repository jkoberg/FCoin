module Tests

open NUnit.Framework

module BigIntToByteArrayTests =

    let [<Test>] toBigInt () = 
      Assert.True ((Convert.BigEndian.toBigInt "\x01\x00"B) = (bigint 256))

    let [<Test>] fromBigInt () =
      Assert.True ((Convert.BigEndian.fromBigInt (bigint 256)) = "\x01\x00"B)

module Base58Tests =

    let [<Test>] VerifyBase58Check () =
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
            match Conv.Base58.verifyBase58check encodedString with
            | Some (version, payload) ->
                Assert.True((version=expectedVersion))
            | None ->
                Assert.Fail("failed verifying base58 string ({0}) {1} ", description, encodedString)
