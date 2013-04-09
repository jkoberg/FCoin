module Keystore

open System.Collections.Generic
open System.Security.Cryptography

let asNullable v = new System.Nullable<_>(v)

let keyParams =
  new CngKeyCreationParameters(
    ExportPolicy = asNullable CngExportPolicies.AllowPlaintextExport
    )

let keys = new Dictionary<string, CngKey>()

let newPrivateKey() =
    let k = CngKey.Create(CngAlgorithm.ECDsaP256, null, keyParams)
    let id = k.UniqueName
    keys.[id] <- k
    k

let withKey x = keys.Item x