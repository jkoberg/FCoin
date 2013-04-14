﻿module Conv.Bitcoin

open EcDsa
open Crypto
open Conv.Bytes
open Conv.Base58

let toUncompressedPubKey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> "\x00"B
  | Point(x, y) -> "\x04"B ++ uint256 x ++ uint256 y

let toCompressedPubkey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> "\x00"B
  | Point(x,y) when y.IsEven -> "\x02"B ++ (uint256 x)
  | Point(x,y) -> "\x03"B ++ (uint256 x)

let fromEncodedPubkey (encoded:byte[]) =
  match encoded.[0] with
  | 0x00uy -> PointO
  | 0x02uy -> secp256k1.fromCompressed false (toBigInt encoded.[1..]) 
  | 0x03uy -> secp256k1.fromCompressed true (toBigInt encoded.[1..])
  | 0x04uy -> Point(toBigInt encoded.[1..32], toBigInt encoded.[33..])
  | _ -> failwith "Unknown key prefix byte"


let toPubKeyHex = toUncompressedPubKey >> Conv.Hex.fromBytes

let toAddressFormat = toUncompressedPubKey >> sha256 >> ripemd160 >> toBase58check 0uy

let toWalletImportFormat  = uint256 >> toBase58check 0x80uy

let fromWalletImportFormat wif : PrivateKey =
  match Base58.verify wif with
  | Some (0x80uy, payload) -> toBigInt payload
  | Some (v, _) -> failwith (sprintf "Valid version 0x%x base58check string, but not the version 0x80 private key expected." v)
  | None -> failwith (sprintf "Bad wallet import string: %s" wif)

let toCompactSignature pubkey (r,s) =
    // create a compact signature (65 bytes), which allows reconstructing the used public key
    // The format is one header byte, followed by two times 32 bytes for the serialized r and s values.
    // The header byte: 0x1B = first key with even y, 0x1C = first key with odd y,
    //                  0x1D = second key with even y, 0x1E = second key with odd y
    match pubkey with 
    | PointO -> failwith "Bad pubkey"
    | Point(x,y) -> 
      "\x1B"B ++ ""B
  

let sign privkey message =
  let hash = sha256 message |> toBigInt
  let signature = secp256k1.sign privkey hash
  signature

//let EcDsaSigRecover key (r,s) msg recid check =
//int ECDSA_SIG_recover_key_GFp(EC_KEY *eckey, ECDSA_SIG *ecsig, const unsigned char *msg, int msglen, int recid, int check)
//{
//    if (!eckey) return 0;

//    int ret = 0;
//    BN_CTX *ctx = NULL;

//    BIGNUM *x = NULL;
//    BIGNUM *e = NULL;
//    BIGNUM *order = NULL;
//    BIGNUM *sor = NULL;
//    BIGNUM *eor = NULL;
//    BIGNUM *field = NULL;
//    EC_POINT *R = NULL;
//    EC_POINT *O = NULL;
//    EC_POINT *Q = NULL;
//    BIGNUM *rr = NULL;
//    BIGNUM *zero = NULL;
//    int n = 0;
//    int i = recid / 2;

//  let i = recid / 2

//    const EC_GROUP *group = EC_KEY_get0_group(eckey);
//    if ((ctx = BN_CTX_new()) == NULL) { ret = -1; goto err; }
//    BN_CTX_start(ctx);
//    order = BN_CTX_get(ctx);
//    if (!EC_GROUP_get_order(group, order, ctx)) { ret = -2; goto err; }
//  let order = n
//    x = BN_CTX_get(ctx);
//    if (!BN_copy(x, order)) { ret=-1; goto err; }
//  let x = order
//    if (!BN_mul_word(x, i)) { ret=-1; goto err; }
//  let x = i * x
//    if (!BN_add(x, x, ecsig->r)) { ret=-1; goto err; }
//  let x = x + r
//    field = BN_CTX_get(ctx);
//    if (!EC_GROUP_get_curve_GFp(group, field, NULL, NULL, ctx)) { ret=-2; goto err; }
//  let field = p
//    if (BN_cmp(x, field) >= 0) { ret=0; goto err; }
//  if x > field then failwith "something" else
//    if ((R = EC_POINT_new(group)) == NULL) { ret = -2; goto err; }
//    if (!EC_POINT_set_compressed_coordinates_GFp(group, R, x, recid % 2, ctx)) { ret=0; goto err; }
//    if (check)
//    {
//    //    if ((O = EC_POINT_new(group)) == NULL) { ret = -2; goto err; }
//    //    if (!EC_POINT_mul(group, O, NULL, R, order, ctx)) { ret=-2; goto err; }
//    //    if (!EC_POINT_is_at_infinity(group, O)) { ret = 0; goto err; }
//    }
//    if ((Q = EC_POINT_new(group)) == NULL) { ret = -2; goto err; }
//    n = EC_GROUP_get_degree(group);
//  let n = size
//    e = BN_CTX_get(ctx);
//    if (!BN_bin2bn(msg, msglen, e)) { ret=-1; goto err; }
//    if (8*msglen > n) BN_rshift(e, e, 8-(n & 7));
//    zero = BN_CTX_get(ctx);
//    if (!BN_zero(zero)) { ret=-1; goto err; }
//    if (!BN_mod_sub(e, zero, e, order, ctx)) { ret=-1; goto err; }
//    rr = BN_CTX_get(ctx);
//    if (!BN_mod_inverse(rr, ecsig->r, order, ctx)) { ret=-1; goto err; }
//    sor = BN_CTX_get(ctx);
//    if (!BN_mod_mul(sor, ecsig->s, rr, order, ctx)) { ret=-1; goto err; }
//    eor = BN_CTX_get(ctx);
//    if (!BN_mod_mul(eor, e, rr, order, ctx)) { ret=-1; goto err; }
//    if (!EC_POINT_mul(group, Q, eor, R, sor, ctx)) { ret=-2; goto err; }
//    if (!EC_KEY_set_public_key(eckey, Q)) { ret=-2; goto err; }

//    ret = 1;

// err:
//    if (ctx) {
//    //    BN_CTX_end(ctx);
//    //    BN_CTX_free(ctx);
//    }
//    if (R != NULL) EC_POINT_free(R);
//    if (O != NULL) EC_POINT_free(O);
//    if (Q != NULL) EC_POINT_free(Q);
//    return ret;
//}
