module EcDsaTests

open EcDsa
open NUnit.Framework

module ECCSignatureTests =
    let priv = 916338557271912060709748568644112591278466742281I
    let pub = Point(1185204462950103058838903871952479866892949799209I, 205560904785823604751039452164442264739289219256I)

    let [<Test>] GeneratePubKey () =
      match bpP160r1.multiply priv bpP160r1.G with
      | None -> Assert.Fail "Returned None instead of the public key point"
      | Some pp -> Assert.True ((pp = pub))

    let [<Test>] OnCurve () =
      Assert.True (bpP160r1.onCurve pub)
      match (bpP160r1.multiply priv bpP160r1.G) with
      | None -> Assert.Fail "Returned None"
      | Some pkey -> Assert.True (bpP160r1.onCurve pkey)

