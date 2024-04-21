/**
 * Author......: See docs/credits.txt
 * License.....: MIT
 */

// https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdfs/pdf_reference_archives/PDFReference.pdf

#ifdef KERNEL_STATIC
#include M2S(INCLUDE_PATH/inc_vendor.h)
#include M2S(INCLUDE_PATH/inc_types.h)
#include M2S(INCLUDE_PATH/inc_platform.cl)
#include M2S(INCLUDE_PATH/inc_common.cl)
#include M2S(INCLUDE_PATH/inc_hash_md5.cl)
#include M2S(INCLUDE_PATH/inc_cipher_rc4.cl)
#endif

#define COMPARE_S M2S(INCLUDE_PATH/inc_comp_single.cl)
#define COMPARE_M M2S(INCLUDE_PATH/inc_comp_multi.cl)

typedef struct pdf
{
  int V;
  int R;
  int P;

  int enc_md;

  u32 id_buf[8];
  u32 u_buf[32];
  u32 o_buf[32];
  u32 u_pass_buf[8];

  int id_len;
  int o_len;
  int u_len;
  int u_pass_len;

  u32 rc4key[2];
  u32 rc4data[2];

} pdf_t;

typedef struct pdf14_tmp
{
  u32 digest[4];
  u32 out[4];

} pdf14_tmp_t;

KERNEL_FQ void m25400_init (KERN_ATTR_TMPS_ESALT (pdf14_tmp_t, pdf_t))
{
  /**
   * base
   */

  const u64 gid = get_global_id (0);
  //const u64 lid = get_local_id (0);

  if (gid >= GID_CNT) return;

  u32 w0[4];

  w0[0] = pws[gid].i[ 0];
  w0[1] = pws[gid].i[ 1];
  w0[2] = pws[gid].i[ 2];
  w0[3] = pws[gid].i[ 3];

  u32 w1[4];

  w1[0] = pws[gid].i[ 4];
  w1[1] = pws[gid].i[ 5];
  w1[2] = pws[gid].i[ 6];
  w1[3] = pws[gid].i[ 7];

  const u32 pw_len = pws[gid].pw_len;

  const u32 padding[8] =
  {
    0x5e4ebf28,
    0x418a754e,
    0x564e0064,
    0x0801faff,
    0xb6002e2e,
    0x803e68d0,
    0xfea90c2f,
    0x7a695364
  };

  /**
   * shared
   */

  u32 P = esalt_bufs[DIGESTS_OFFSET_HOST].P; // TODO this is never used, but should be according according to "Algorithm 3.2 Computing an encryption key" line 4.

  u32 id_buf[12]; // TODO this is never used, but should be according according to "Algorithm 3.2 Computing an encryption key" line 5.

  id_buf[ 0] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[0];
  id_buf[ 1] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[1];
  id_buf[ 2] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[2];
  id_buf[ 3] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[3];

  id_buf[ 4] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[4];
  id_buf[ 5] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[5];
  id_buf[ 6] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[6];
  id_buf[ 7] = esalt_bufs[DIGESTS_OFFSET_HOST].id_buf[7];

  id_buf[ 8] = 0;
  id_buf[ 9] = 0;
  id_buf[10] = 0;
  id_buf[11] = 0;

  u32 rc4data[2];

  rc4data[0] = padding[0];
  rc4data[1] = padding[1];

  /**
   * main init
   */

  u32 w0_t[4];
  u32 w1_t[4];
  u32 w2_t[4];
  u32 w3_t[4];

  // max length supported by pdf11 is 32

  w0_t[0] = padding[0];
  w0_t[1] = padding[1];
  w0_t[2] = padding[2];
  w0_t[3] = padding[3];
  w1_t[0] = padding[4];
  w1_t[1] = padding[5];
  w1_t[2] = padding[6];
  w1_t[3] = padding[7];
  w2_t[0] = 0;
  w2_t[1] = 0;
  w2_t[2] = 0;
  w2_t[3] = 0;
  w3_t[0] = 0;
  w3_t[1] = 0;
  w3_t[2] = 0;
  w3_t[3] = 0;

  switch_buffer_by_offset_le (w0_t, w1_t, w2_t, w3_t, pw_len);

  // add password
  // truncate at 32 is wanted, not a bug!
  // add padding

  w0_t[0] |= w0[0];
  w0_t[1] |= w0[1];
  w0_t[2] |= w0[2];
  w0_t[3] |= w0[3];
  w1_t[0] |= w1[0];
  w1_t[1] |= w1[1];
  w1_t[2] |= w1[2];
  w1_t[3] |= w1[3];
  w2_t[0] = 0x80;
  w2_t[1] = 0;
  w2_t[2] = 0;
  w2_t[3] = 0;
  w3_t[0] = 0;
  w3_t[1] = 0;
  w3_t[2] = 32 * 8;
  w3_t[3] = 0;

  u32 digest[4];

  digest[0] = MD5M_A;
  digest[1] = MD5M_B;
  digest[2] = MD5M_C;
  digest[3] = MD5M_D;

  md5_transform (w0_t, w1_t, w2_t, w3_t, digest);

  tmps[gid].digest[0] = digest[0];
  tmps[gid].digest[1] = digest[1];
  tmps[gid].digest[2] = digest[2];
  tmps[gid].digest[3] = digest[3];

  tmps[gid].out[0] = rc4data[0];
  tmps[gid].out[1] = rc4data[1];
  tmps[gid].out[2] = 0;
  tmps[gid].out[3] = 0;
}

KERNEL_FQ void m25400_loop (KERN_ATTR_TMPS_ESALT (pdf14_tmp_t, pdf_t))
{
  /**
   * base
   */

  const u64 gid = get_global_id (0);
  const u64 lid = get_local_id (0);

  if (gid >= GID_CNT) return;

  /**
   * shared
   */

  LOCAL_VK u32 S[64 * FIXED_LOCAL_SIZE];

  /**
   * loop
   */

  u32 digest[4];
  digest[0] = tmps[gid].digest[0];
  digest[1] = tmps[gid].digest[1];
  digest[2] = tmps[gid].digest[2];
  digest[3] = tmps[gid].digest[3];

  u32 out[4];
  out[0] = tmps[gid].out[0];
  out[1] = tmps[gid].out[1];
  out[2] = tmps[gid].out[2];
  out[3] = tmps[gid].out[3];

  for (u32 i = 0, j = LOOP_POS; i < LOOP_CNT; i++, j++)
  {
    if (j < 50)
    {
      // the owner-key is generated by iterating a md5 hash 50 times
      //  see: "Algorithm 3.3 Computing the encryption dictionary’s O (owner password) value"
      u32 w0_t[4];
      u32 w1_t[4];
      u32 w2_t[4];
      u32 w3_t[4];

      w0_t[0] = digest[0];
      w0_t[1] = digest[1];
      w0_t[2] = digest[2];
      w0_t[3] = digest[3];
      w1_t[0] = 0x80;
      w1_t[1] = 0;
      w1_t[2] = 0;
      w1_t[3] = 0;
      w2_t[0] = 0;
      w2_t[1] = 0;
      w2_t[2] = 0;
      w2_t[3] = 0;
      w3_t[0] = 0;
      w3_t[1] = 0;
      w3_t[2] = 16 * 8;
      w3_t[3] = 0;

      digest[0] = MD5M_A;
      digest[1] = MD5M_B;
      digest[2] = MD5M_C;
      digest[3] = MD5M_D;

      md5_transform (w0_t, w1_t, w2_t, w3_t, digest);
    }
  }

  out[0] = esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[0]; // store original o-value in out (scratchpad)
  out[1] = esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[1];
  out[2] = esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[2];
  out[3] = esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[3];
  u32 o_rc4_decryption_key[4];
  o_rc4_decryption_key[0] = digest[0]; // store the owner-key
  o_rc4_decryption_key[1] = digest[1];
  o_rc4_decryption_key[2] = digest[2];
  o_rc4_decryption_key[3] = digest[3];

  // we decrypt the o-value to obtain either the owner-password (or user-password if no owner-password is set)
  //  see: "Algorithm 3.3 Computing the encryption dictionary’s O (owner password) value": "If there is no owner password, use the user password instead".
  u32 tmp[4];
  for (u32 i = 19; i>0; i--)
  {
    // xor the iterator into the rc4 key
    const u32 xv = i <<  0
                 | i <<  8
                 | i << 16
                 | i << 24;

    tmp[0] = o_rc4_decryption_key[0] ^ xv;
    tmp[1] = o_rc4_decryption_key[1] ^ xv;
    tmp[2] = o_rc4_decryption_key[2] ^ xv;
    tmp[3] = o_rc4_decryption_key[3] ^ xv;

    rc4_init_128 (S, tmp, lid);
    rc4_next_16 (S, 0, 0, out, out, lid);
  }

  rc4_init_128 (S, o_rc4_decryption_key, lid);
  rc4_next_16 (S, 0, 0, out, out, lid); // output of the rc4 decrypt of the o-value should be the padded user-password

  tmps[gid].digest[0] = digest[0];
  tmps[gid].digest[1] = digest[1];
  tmps[gid].digest[2] = digest[2];
  tmps[gid].digest[3] = digest[3];

  tmps[gid].out[0] = out[0];
  tmps[gid].out[1] = out[1];
  tmps[gid].out[2] = out[2];
  tmps[gid].out[3] = out[3];
}

KERNEL_FQ void m25400_comp (KERN_ATTR_TMPS_ESALT (pdf14_tmp_t, pdf_t))
{
  const u32 digest[4] =
  {
    esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[0],
    esalt_bufs[DIGESTS_OFFSET_HOST].o_buf[1],
    0x0,// apparently only the first 16 bytes of the digest are used to look it up?
    0x0 // apparently only the first 16 bytes of the digest are used to look it up?
  };

  const u32 padding[8] =
  {
    0x5e4ebf28,
    0x418a754e,
    0x564e0064,
    0x0801faff,
    0xb6002e2e,
    0x803e68d0,
    0xfea90c2f,
    0x7a695364
  };

  /**
   * modifier
   */
  const u64 gid = get_global_id (0);

  if (gid >= GID_CNT) return;

  const u64 lid = get_local_id (0);


  #define il_pos 0


  const u32 out[4] =
  {
    tmps[gid].out[0],
    tmps[gid].out[1],
    tmps[gid].out[2],
    tmps[gid].out[3]
  };


  // the best comparison I can think of is checking each byte
  //  whether it's a padding byte or ASCII, if so we're good,
  //  if not, decryption was not successful

  bool correct = true;

  int i_padding=0;

  for (int i = 0; i < 16; i++)
  {
    // cast out buffer to byte such that we can do a byte per byte comparison
    PRIVATE_AS const u32 *u32OutBufPtr = (PRIVATE_AS u32 *) out;
    PRIVATE_AS const u8  *u8OutBufPtr  = (PRIVATE_AS u8  *) u32OutBufPtr;

    // cast padding buffer to byte such that we can do a byte per byte comparison
    PRIVATE_AS const u32 *u32OutPadPtr = (PRIVATE_AS u32 *) padding;
    PRIVATE_AS const u8  *u8OutPadPtr  = (PRIVATE_AS u8  *) u32OutPadPtr;

    // we don't use the user-password in the attack now (as we don't need it),
    //  however we could use it in the comparison of the decrypted o-value,
    //  yet it may make this attack a bit more fragile, as now we just check for ASCII
    if ((u8OutBufPtr[i] >= 20 && u8OutBufPtr[i] <= 0x7e) ||
        (u8OutBufPtr[i] == u8OutPadPtr[i_padding]))
    {
      if (u8OutBufPtr[i] == u8OutPadPtr[i_padding])
      {
        //printf("correct padding byte[%d]=0x%02x\n", i, u8OutBufPtr[i]);
        i_padding = i_padding + 1;
      }
      else
      {
        if (u8OutBufPtr[i] >= 20 && u8OutBufPtr[i] <= 0x7e)
        {
          //printf("correct ASCII byte[%d]=0x%02x\n", i, u8OutBufPtr[i]);
        }
      }
    }
    else
    {
      //printf("wrong byte[%d]=0x%02x\n", i, u8OutBufPtr[i]);
      //
      //printf("u8OutBufPtr=0x");
      //for(int j=0;j<16;j++) {
      //  printf("%02x", u8OutBufPtr[j]);
      //}
      //printf("\n");
      //
      //printf("u8OutPadPtr=0x");
      //for(int j=0;j<16;j++) {
      //  printf("%02x", u8OutPadPtr[j]);
      //}
      //printf("\n");

      correct = false;
      break;
    }
  }

  if (correct)
  {
    int digest_pos = find_hash (digest, DIGESTS_CNT, &digests_buf[DIGESTS_OFFSET_HOST]);

    if (digest_pos != -1)
    {
      const u32 final_hash_pos = DIGESTS_OFFSET_HOST + digest_pos;

      if (hc_atomic_inc (&hashes_shown[final_hash_pos]) == 0)
      {
        mark_hash (plains_buf, d_return_buf, SALT_POS_HOST, DIGESTS_CNT, digest_pos, final_hash_pos, gid, il_pos, 0, 0);
      }
    }
  }
}
