(* Some Code copied from encodeMap.ml *)
type stream =
{
  buf : Buffer.t;
  patches : (int * char) list ref
}

let stream () = {buf = Buffer.create 8192; patches = ref []}
let pos s = Buffer.length s.buf
let put s b = Buffer.add_char s.buf b
let put_string s bs = Buffer.add_string s.buf bs
let patch s pos b = s.patches := (pos, b) :: !(s.patches)

let to_string s =
  let bs = Buffer.to_bytes s.buf in
  List.iter (fun (pos, b) -> Bytes.set bs pos b) !(s.patches);
  Bytes.to_string bs


let encode (actor_messages : int32 list): string =
  let s = stream () in

  let u8 i = put s (Char.chr (i land 0xff)) in
  let u16 i = u8 (i land 0xff); u8 (i lsr 8) in
  let u32 i =
    Int32.(u16 (to_int (logand i 0xffffl));
           u16 (to_int (shift_right i 16))) in

  let rec vu64 i =
    let b = Int64.(to_int (logand i 0x7fL)) in
    if 0L <= i && i < 128L then u8 b
    else (u8 (b lor 0x80); vu64 (Int64.shift_right_logical i 7)) in

  let vu32 i = vu64 Int64.(logand (of_int32 i) 0xffffffffL) in

  let gap32 () = let p = pos s in u32 0l; u8 0; p in
  let patch_gap32 p n =
    assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
    let lsb i = Char.chr (i land 0xff) in
    patch s p (lsb (n lor 0x80));
    patch s (p + 1) (lsb ((n lsr 7) lor 0x80));
    patch s (p + 2) (lsb ((n lsr 14) lor 0x80));
    patch s (p + 3) (lsb ((n lsr 21) lor 0x80));
    patch s (p + 4) (lsb (n lsr 28)) in


  let string bs = vu32 (Int32.of_int (String.length bs)); put_string s bs in

  let section id f =
    u8 id;
    let g = gap32 () in
    let p = pos s in
    f ();
    patch_gap32 g (pos s - p)
    in

(* End of code copy *)

  section 0 (fun _ ->
    string "types";
    vu32 1l;
    vu32 0x60l; (* function type op code *)
    vu32 2l; (* two args *)
    vu32 0x7fl; vu32 0x7fl; (* both int32 *)
    vu32 0l;
  );
  section 0 (fun _ ->
    string "typeMap";
    vu32 (Int32.of_int (List.length actor_messages));
    List.iter (fun fi ->
      vu32 fi;
      vu32 0l;
    ) actor_messages
  );
  to_string s

