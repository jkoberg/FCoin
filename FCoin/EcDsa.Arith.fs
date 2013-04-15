module EcDsa.Arith

type Point = PointO | Point of  bigint * bigint

type Curve = {a:bigint; b:bigint; p:bigint; G:Point; n:bigint; h:bigint; size:int}

type PrivateKey = bool * bigint

type PublicKey = bool * Point

type MaybeError<'a> = 
      | Valid of 'a
      | Error of string

let (%) x m = if x < 0I then (x % m) + m else x % m

let pow = bigint.ModPow

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


