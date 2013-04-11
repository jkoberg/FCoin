module EcDsa.Arith

type Point = PointO | Point of  bigint * bigint
type Curve = {p: bigint; a: bigint;  b: bigint}
type PrivateKey = bigint
type PublicKey = Point

let (%) x m = if x < 0I then (x % m) + m else x % m

let rec egcd a b = 
    if b = 0I then (a, 1I, 0I) else
    let q, r = bigint.DivRem(a, b)
    let d, s, t = egcd b r
    (d, t, s - (q * t))

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
        let xr = ((l ** 2) - (2I * x)) % c.p
        let yr = ((l * (x - xr)) - y) % c.p
        Point(xr, yr)

let add (c: Curve) pj pk =
    match pj, pk with
    | PointO, k  -> k
    | j, PointO  -> j
    | j, k when j = k -> double c j
    | Point(x1, y1), Point(x2, y2) when x1 = x2 && y1 = -y2 -> PointO
    | Point(xj, yj), Point(xk, yk) ->
        let l = modDiv ((yj - yk) % c.p) ((xj - xk) % c.p) c.p
        let xr = ((l ** 2) - xj - xk) % c.p
        let yr = ((l * (xj - xr)) - yj) % c.p
        Point(xr, yr)

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

let newPrivKey n : PrivateKey = (Random.bytes 32 |> Conv.Bytes.toBigInt) % n

let getPubKey  (c:Curve) g (p:PrivateKey) : PublicKey = multiply c p g
