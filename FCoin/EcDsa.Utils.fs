module EcDsa.Utils
    open Arith

    let onCurve (c: Curve) pt = 
        match pt with
        | PointO -> true
        | Point(x,y) -> pow(y, 2I, c.p)  =  (pow(x, 3I, c.p) + (c.a * x) + c.b) % c.p

    let fromCompressed (c:Curve) isYOdd (x:bigint) : PublicKey = 
        let ySquared = (pow(x, 3I, c.p)  +  c.a * pow(x, 2I, c.p)  +  c.b) % c.p
        let ytrial = pow(ySquared, ((c.p+1I) / 4I), c.p)
        if isYOdd = ytrial.IsEven
        then true, Point(x, c.p - ytrial)
        else true, Point(x, ytrial)

    let rec newPrivKey (c:Curve) compressed : PrivateKey = 
        let r = Crypto.randBits c.size
        if r > 0I && r < c.n then (compressed, r) else newPrivKey c compressed

    let getPubKey c ((compressed,k):PrivateKey) : PublicKey = (compressed, multiply c k c.G)

    let rec newKeypair c compressed =
        let k = newPrivKey c compressed
        match getPubKey c k with
        | _, PointO -> newKeypair c compressed
        | _, Point(r,_) when r % c.n = 0I -> newKeypair c compressed
        | _, Point(r,_) as kG -> (k, kG, r)

    let rec ecdsa_sign (c:Curve) privkey hash = 
        let z = hash
        let (_,k), (_,kG), r = newKeypair c false
        let s = ((modInv k c.n) * ((z + ((r * privkey)%c.n))%c.n) ) % c.n
        if s = 0I then failwith "bzzt" else
        (r, s)
        
    let ecdsa_verify (c:Curve) pubkey hash (r,s) =
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
        | _ as p -> Valid p




