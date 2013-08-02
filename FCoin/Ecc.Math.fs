namespace Ecc

open Ecc.Arith

type Math (c:CurveParams) =
  let ( ** ) a b = bigint.ModPow(a, b, c.p)
  let ( / ) a b = modDiv a b c.p
  let ( - ) a b = (a - b) % c.p

  member this.Curve = c

  member this.double p =
    match p with
    | PointO -> PointO
    | Point(x, y) ->
      let s = ((3I * (x ** 2I)) + c.a) / (2I * y)
      let newx = ((s ** 2I) - (2I * x) - c.a) // wikipedia
      let newy = ((s * (x - newx)) - y)
      Point(newx, newy)

  member this.add p1 p2 =
    match p1, p2 with
    | PointO, p2  -> p2
    | p1, PointO  -> p1
    | p1, p2 when p1 = p2 -> this.double p1
    | Point(x1, y1), Point(x2, y2) when x1 = x2 && y1 = -y2 -> PointO
    | Point(x1, y1), Point(x2, y2) when x1 = x2 -> PointO ///??? //failwith "How to divide by 0???"
    | Point(x1, y1), Point(x2, y2) ->
      let s = (y1 - y2) / (x1 - x2)
      let newx = ((s ** 2I) - c.a - x1 - x2) // wikipedia
      let newy = ((s * (x1 - newx)) - y1)
      Point(newx, newy)

  member this.multiply (n:bigint) point =
    if n.IsZero 
      then PointO 
      else if n.IsEven
        then this.multiply (n / 2I) (this.double point)
        else this.add point (this.multiply (n - 1I) point)

  member this.onCurve pt = 
    match pt with
    | PointO -> true
    | Point(x,y) -> (y ** 2I)  =  ((x ** 3I) + (c.a * x) + c.b) % c.p

  member this.fromCompressed isYOdd (x:bigint) : PublicKey = 
    let ySquared = ((x ** 3I)  +  c.a * (x ** 2I)  +  c.b) % c.p
    let ytrial = (ySquared ** ((c.p + 1I) / 4I))
    if isYOdd = ytrial.IsEven
    then true, Point(x, c.p - ytrial)
    else true, Point(x, ytrial)

  member this.newPrivKey compressed : PrivateKey = 
    let r = Crypto.randBits c.size
    if r > 0I && r < c.n then (compressed, r) else this.newPrivKey compressed

  member this.getPubKey ((compressed,k):PrivateKey) : PublicKey = (compressed, this.multiply k c.G)

  member this.newKeypair compressed =
    let k = this.newPrivKey compressed
    match this.getPubKey k with
    | _, PointO -> this.newKeypair compressed
    | _, Point(r,_) when r % c.n = 0I -> this.newKeypair compressed
    | _, Point(r,_) as kG -> (k, kG, r)

