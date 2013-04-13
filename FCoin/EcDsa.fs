module EcDsa

type Point = PointO | Point of  bigint * bigint
type Curve = {p: bigint; a: bigint;  b: bigint}
type PrivateKey = bigint
type PublicKey = Point

let (%) x m = if x < 0I then (x % m) + m else x % m

let rec egcd n1 n2 = 
    if n2 = 0I then (n1, 1I, 0I) else
    let quo, rem = bigint.DivRem(n1, n2)
    let d, s, t = egcd n2 rem
    (d, t, s - (quo * t))

let modInv a m =
    match egcd a m with
    | x, y, _ when x = 1I -> y % m
    | _ -> failwith "Invalid point in EcDsa curve reached"
    
let modDiv a b p = (a * (modInv b p)) % p

let double (c: Curve) p =
    match p with
    | PointO -> PointO
    | Point(x, y) ->
        let l = modDiv ((3I * (x ** 2)) + c.a) (2I * y) c.p
        let newx = ((l ** 2) - (2I * x)) % c.p
        let newy = ((l * (x - newx)) - y) % c.p
        Point(newx, newy)

let add (c: Curve) p1 p2 =
    match p1, p2 with
    | PointO, p2  -> p2
    | p1, PointO  -> p1
    | p1, p2 when p1 = p2 -> double c p1
    | Point(x1, y1), Point(x2, y2) when x1 = x2 && y1 = -y2 -> PointO
    | Point(x1, y1), Point(x2, y2) ->
        let l = modDiv ((y1 - y2) % c.p) ((x1 - x2) % c.p) c.p
        let newx = ((l ** 2) - x1 - x2) % c.p
        let newy = ((l * (x1 - newx)) - y1) % c.p
        Point(newx, newy)

let rec multiply curve (n:bigint) point =
    if n.IsZero 
      then PointO 
      else if n.IsEven
        then multiply curve (n / 2I) (double curve point)
        else add curve point (multiply curve (n - 1I) point)

let onCurve (c: Curve) pt = 
    match pt with
    | PointO -> true
    | Point(x,y) ->
        bigint.ModPow(y, 2I, c.p)  =  (bigint.ModPow(x, 3I, c.p) + (c.a * x) + c.b) % c.p


module secp256k1 =
    open Conv.UnsignedBig

    let size = 256
    let p = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"
    let a = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let b = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000007"
    let G = Point(fromHex "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798",
                  fromHex "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8")
    let n = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141"
    let h = 1I
    let curve = {a = a; b = b; p = p}

    let double = double curve
    let add = add curve
    let multiply = multiply curve
    let onCurve = onCurve curve
    let getPubKey privkey = multiply privkey G

    let rec newPrivKey () : PrivateKey = 
      let r = Crypto.randBits size
      if r < n then r else newPrivKey()

