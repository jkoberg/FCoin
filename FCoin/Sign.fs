module Sign
open System
open System.Security.Cryptography


let asNullable v = new Nullable<_>(v)

let keyParams =
  new CngKeyCreationParameters(
    ExportPolicy = asNullable CngExportPolicies.AllowPlaintextExport
    )

let newDsaKey () =
  let k = CngKey.Create(CngAlgorithm.ECDsaP256, null, keyParams)
  k.Export(CngKeyBlobFormat.Pkcs8PrivateBlob)


let verify publickey signature (data:byte[]) =
  let k = CngKey.Create(CngAlgorithm.ECDsaP256, null, keyParams)
  use dsa = new ECDsaCng(k)
  dsa.VerifyData(data, signature)

let sign privatekey (data:byte[]) =
  let k = CngKey.Import(privatekey, CngKeyBlobFormat.EccPrivateBlob)
  use dsa = new ECDsaCng(k)
  dsa.SignData(data)


let unpackPublic txt = 
  match Base58.verifyBase58check txt with
  | Some (0x04uy, data) ->
    let x = Radix.BigEndian.toBigInt data.[..31]
    let y = Radix.BigEndian.toBigInt data.[32..]
    Some (x,y)
  | _ -> None
          