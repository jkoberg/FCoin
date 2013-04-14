module EcDsa

type Point = PointO | Point of  bigint * bigint
type Curve = {a:bigint; b:bigint; p:bigint; G:Point; n:bigint; h:bigint; size:int}
type PrivateKey = bigint
type PublicKey = Point
type MaybeError<'a> = 
      | Valid of 'a
      | Error of string

let (%) x m = if x < 0I then (x % m) + m else x % m

let rec egcd n1 n2 = 
    if n2 = 0I then (n1, 1I, 0I) else
    let quo, rem = bigint.DivRem(n1, n2)
    let d, s, t = egcd n2 rem
    (d, t, s - (quo * t))

let modInv a m =
    match egcd a m with
    | x, y, _ when x = 1I -> y % m
    | x,y,r -> 
        failwith "Invalid point in EcDsa curve reached"
    
let modDiv a b p = (a * (modInv b p)) % p

let double (c: Curve) p =
    match p with
    | PointO -> PointO
    | Point(x, y) ->
        let s = modDiv ((3I * (x ** 2)) + c.a) (2I * y) c.p
        let newx = ((s ** 2) - (2I * x) - c.a) % c.p // wikipedia
        let newy = ((s * (x - newx)) - y) % c.p
        Point(newx, newy)

let add (c: Curve) p1 p2 =
    match p1, p2 with
    | PointO, p2  -> p2
    | p1, PointO  -> p1
    | p1, p2 when p1 = p2 -> double c p1
    | Point(x1, y1), Point(x2, y2) when x1 = x2 && y1 = -y2 -> PointO
    | Point(x1, y1), Point(x2, y2) when x1 = x2 -> PointO ///??? //failwith "How to divide by 0???"
    | Point(x1, y1), Point(x2, y2) ->
        let s = modDiv ((y1 - y2) % c.p) ((x1 - x2) % c.p) c.p
        let newx = ((s ** 2) - c.a - x1 - x2) % c.p // wikipedia
        let newy = ((s * (x1 - newx)) - y1) % c.p
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

let fromCompressed (c:Curve) isYOdd x : PublicKey = 
    let ySquared = (bigint.ModPow(x, 3I, c.p)  +  c.a * bigint.ModPow(x, 2I, c.p)  +  c.b) % c.p
    let ytrial = bigint.ModPow(ySquared, ((c.p+1I) / 4I), c.p)
    if isYOdd && ytrial.IsEven
      then Point(x, c.p - ytrial)
      else Point(x, ytrial)


let rec newPrivKey (c:Curve) : PrivateKey = 
    let r = Crypto.randBits c.size
    if r > 0I && r < c.n then r else newPrivKey c


let getPubKey c k : PublicKey = multiply c k c.G

let rec newKeypair c =
    let k = newPrivKey c
    match getPubKey c k with
    | PointO -> newKeypair c
    | Point(r,_) when r % c.n = 0I -> newKeypair c
    | Point(r,_) as kG -> (k, kG, r)

let rec sign (c:Curve) privkey hash = 
    let z = hash
    let k, kG, r = newKeypair c
    let s = ((modInv k c.n) * ((z + ((r * privkey)%c.n))%c.n) ) % c.n
    if s = 0I then sign c privkey hash else
    (r, s)
        
let verify (c:Curve) pubkey hash (r,s) =
  let outOfRange i = i < 1I || i > c.n
  if outOfRange r || outOfRange s then Error "Invalid signature" else 
  match pubkey with
  | PointO -> Error "invalid pubkey"
  | pubkey when not (onCurve c pubkey) -> Error "invalid pubkey"
  | pubkey when (multiply c c.n pubkey) <> PointO -> Error "invalid pubkey"
  | pubkey ->
    let z = hash
    let w = modInv s c.n
    let u1 = (z * w) % c.n
    let u2 = (r * w) % c.n
    match add c (multiply c u1 c.G) (multiply c u2 pubkey) with
    | PointO -> Error "Signature didn't match"
    | Point(x1, _) when r <> (x1 % c.n) -> Error "Signature didn't match"
    | _ -> Valid hash




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
    let curve = {a=a; b=b; p=p; G=G; n=n; h=h; size=size}

    let double = double curve
    let add = add curve
    let multiply = multiply curve
    let onCurve = onCurve curve
    let fromCompressed = fromCompressed curve
    let newPrivKey () = newPrivKey curve
    let getPubKey = getPubKey curve
    let newKeypair () = newKeypair curve
    let sign = sign curve
    let verify = verify curve
