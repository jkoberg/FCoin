module Random

let Rng = System.Security.Cryptography.RNGCryptoServiceProvider.Create()

let bytes count =
  let mutable out:byte[] = Array.zeroCreate count
  do Rng.GetBytes(out)
  out
