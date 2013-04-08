module EcDsa

type Point =
 | PointO 
 | Point of  bigint * bigint

type Curve = {p: bigint; a: bigint;  b: bigint}

type PrivateKey = bigint

type PublicKey = Point

let fromHex s =
  let bytes = System.Runtime.Remoting.Metadata.W3cXsd2001.SoapHexBinary.Parse(s).Value
  let littleEndian = Array.append (Array.rev bytes) "\x00"B
  (bigint littleEndian)

let (%!) x m =
  if x < 0I then
    (x % m) + m
  else
    x % m

module Arith =

    let rec egcd a b = 
      if b = 0I then
        (a, 1I, 0I)
      else
        let q, r = bigint.DivRem(a, b)
        let d, s, t = egcd b r
        (d, t, s - (q * t))

    let modInv a m = 
      let x, y, _ = egcd a m
      if x = 1I then
        Some (y %! m)
      else
        failwith (sprintf "No modular inverse of %A mod %A" a m)
        None
    
    let modDiv a b p =
        match modInv b p with
        | None -> None
        | Some invertedB -> Some ((a * invertedB) %! p)

    let double (c: Curve) p =
      match p with
      | PointO -> Some PointO
      | Point(x, y) ->
        let ln = (3I * (x ** 2)) + c.a
        let ld = 2I * y
        match modDiv ln ld c.p with
        | None -> None
        | Some l ->
            let xr = ((l ** 2) - (2I * x)) %! c.p
            let yr = ((l * (x - xr)) - y) %! c.p
            Some (Point(xr, yr))

    let add (c: Curve) pj pk =
      match pj, pk with
      | PointO, k  -> Some k
      | j, PointO  -> Some j
      | j, k when j = k -> double c j
      | Point(x1, y1), Point(x2, y2) when x1 = x2 && y1 = -y2 -> Some PointO
      | Point(xj, yj), Point(xk, yk) ->
          match modDiv ((yj - yk) %! c.p) ((xj - xk) %! c.p) c.p with
          | None -> None
          | Some lam ->
            let xr = ((lam ** 2) - xj - xk) %! c.p
            let yr = ((lam * (xj - xr)) - yj) %! c.p
            Some (Point(xr, yr))

    let rec multiply curve (n:bigint) point =
      if n.IsZero then
        Some PointO
      else
          if n.IsEven then
            match double curve point with
            | None -> None
            | Some doubledPoint -> multiply curve (n / 2I) doubledPoint
          else
            match multiply curve (n - 1I) point with
            | None -> None
            | Some otherPoint -> add curve point otherPoint

    let onCurve (c: Curve) pt = 
      match pt with
      | PointO -> true
      | Point(x,y) ->
        let q = bigint.ModPow(y, 2I, c.p)
        let qq = (bigint.ModPow(x, 3I, c.p) + (c.a * x) + c.b) %! c.p
        q = qq

    let getPubKey  (c:Curve) g (p:bigint) = multiply c p g


module secp256k1 =
    let size = 256
    let p = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"
    let a = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let b = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000007"
    let G = Point((fromHex "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798"),
                  (fromHex "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8"))
    let n = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141"
    let h = 1I
    let curve = {a = a; b = b; p = p}

    let double = Arith.double curve
    let add = Arith.add curve
    let multiply = Arith.multiply curve
    let onCurve = Arith.onCurve curve
    let getPubKey = Arith.getPubKey curve G


module bpP160r1 =
    let size = 256
    let a = fromHex "340E7BE2 A280EB74 E2BE61BA DA745D97 E8F7C300"
    let b = fromHex "1E589A85 95423412 134FAA2D BDEC95C8 D8675E58"
    let p = fromHex "E95E4A5F 737059DC 60DFC7AD 95B3D813 9515620F"
    let G = Point((fromHex "BED5AF16 EA3F6A4F 62938C46 31EB5AF7 BDBCDBC3"),
                  (fromHex "1667CB47 7A1A8EC3 38F94741 669C9763 16DA6321"))
    let n = fromHex "E95E4A5F 737059DC 60DF5991 D4502940 9E60FC09"
    let h = 1I
    let curve = {a=a; b=b; p=p}

    let double = Arith.double curve
    let add = Arith.add curve
    let multiply = Arith.multiply curve
    let onCurve = Arith.onCurve curve 
    let getPubKey = Arith.getPubKey curve G



