module BitcoinAddressTests

open EcDsa

open NUnit.Framework

module SampleAddressTests =
    let examples = [
      "1JzGJw28sJSJJyDxfAgnVNhhLSLqnbMSMG","5JSoYATm69sDX3qwyxau1EC38BQU6BYAn9p38HdrgXFkKF2tLmx"
      "13ZdTrzSxDnDdvqGvLzG3kSdByXSLGepos","5JdkR3BXCn7kL9Az432U1mGPT7o8kpwNxsX9vFeGcseapY8wo9Z"
      "18Mw4t4GbJeBBvUHep71RUwQByCeKPdR8p","5JjgKcw8aRdKnRTFs5drSo5z5ZyBYCSFWtZ29ELnCjdgAzH9qQq"
      ]

    let [<Test>] CheckSamples () = 
        for address, privkey in examples do
            match Conv.Base58.verifyBase58check address with
            | None -> failwith "Bad address base58"
            | Some (addrmagic, addrdata) ->
                match Conv.Base58.verifyBase58check privkey with
                | None -> failwith "Bad privkey base58"
                | Some (privmagic, privdata) ->
                    let priv = Conv.UnsignedBig.fromBytes privdata
                    let addr = Conv.Bitcoin.toAddressFormat priv
                    Assert.AreEqual(address, addr, "Failed to correctly generate address from privkey")
                    Assert.AreEqual(privkey, Conv.Bitcoin.toWalletImportFormat (Conv.UnsignedBig.fromBytes privdata), "Failed to encode privkey as WIF")




