
module EcDsa.secp256k1
    open Conv.UnsignedBig
    open Arith

    let size = 256
    let p = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"
    let a = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let b = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000007"
    let G = Point(fromHex "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798",
                  fromHex "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8")
    let n = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141"
    let h = 1I
    let curve = {a=a; b=b; p=p; G=G; n=n; h=h; size=size}

    let double = double curve
    let add = add curve
    let multiply = multiply curve

    let onCurve = Utils.onCurve curve
    let fromCompressed = Utils.fromCompressed curve
    let newPrivKey compressed = Utils.newPrivKey curve compressed
    let getPubKey = Utils.getPubKey curve
    let newKeypair compressed  = Utils.newKeypair curve compressed
    let ecdsa_sign = Utils.ecdsa_sign curve
    let ecdsa_verify = Utils.ecdsa_verify curve

