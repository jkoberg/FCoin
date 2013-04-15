module BitcoinAddressTests

open EcDsa
open Conv

open NUnit.Framework

module SampleAddressTests =

    let examples = [
      "1JzGJw28sJSJJyDxfAgnVNhhLSLqnbMSMG","5JSoYATm69sDX3qwyxau1EC38BQU6BYAn9p38HdrgXFkKF2tLmx"
      "13ZdTrzSxDnDdvqGvLzG3kSdByXSLGepos","5JdkR3BXCn7kL9Az432U1mGPT7o8kpwNxsX9vFeGcseapY8wo9Z"
      "18Mw4t4GbJeBBvUHep71RUwQByCeKPdR8p","5JjgKcw8aRdKnRTFs5drSo5z5ZyBYCSFWtZ29ELnCjdgAzH9qQq"
      ]

    let [<Test>] CheckSamples () = 
        for address, privkey in examples do
            match Base58.verify address with
            | None -> failwith "Bad address base58"
            | Some (addrmagic, addrdata) ->
                match Base58.verify privkey with
                | None -> failwith "Bad privkey base58"
                | Some (privmagic, privdata) ->
                    let priv = false, UnsignedBig.fromBytes privdata
                    let pub = EcDsa.secp256k1.getPubKey priv
                    let addr = Bitcoin.Pubkey.toAddress pub
                    Assert.AreEqual(address, addr, "Failed to correctly generate address from privkey")
                    Assert.AreEqual(privkey, Bitcoin.Privkey.toWalletImportFormat priv, "Failed to encode privkey as WIF")




