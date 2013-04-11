module Sign
open System
open System.Security.Cryptography


let verify publickey signature (data:byte[]) =
  let k = CngKey.Import(publickey, CngKeyBlobFormat.EccPublicBlob)
  use dsa = new ECDsaCng(k)
  dsa.VerifyData(data, signature)

let sign privatekey (data:byte[]) =
  let k = CngKey.Import(privatekey, CngKeyBlobFormat.EccPrivateBlob)
  use dsa = new ECDsaCng(k)
  dsa.SignData(data)


let unpackPublic txt = 
  match Conv.Base58.verifyBase58check txt with
  | Some (0x04uy, data) ->
    let x = Conv.UnsignedBig.fromBytes data.[..31]
    let y = Conv.UnsignedBig.fromBytes data.[32..]
    Some (x,y)
  | _ -> None
          