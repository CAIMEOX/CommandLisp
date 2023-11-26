type term =
  | Byte of int
  | Short of int
  | Int of int32
  | Long of int64
  | Float of float
  | Double of float
  | ByteArray of int array
  | String of string
  | List of term list
  | Compound of nbt list
  | IntArray of int array
  | LongArray of int64 array

and nbt = string * term

let term_to_tag term =
  match term with
  | Byte _ -> '\x01'
  | Short _ -> '\x02'
  | Int _ -> '\x03'
  | Long _ -> '\x04'
  | Float _ -> '\x05'
  | Double _ -> '\x06'
  | ByteArray _ -> '\x07'
  | String _ -> '\x08'
  | List _ -> '\x09'
  | Compound _ -> '\x0a'
  | IntArray _ -> '\x0b'
  | LongArray _ -> '\x0c'

let end_tag = '\x00'

let read_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  buf

let write_tag buf tag =
  Buffer.add_uint16_le buf (String.length tag);
  Buffer.add_string buf tag

let rec write_header buf nbt = Buffer.add_char buf (term_to_tag (snd nbt))

let encode_nbt nbt =
  let buf = Buffer.create 0 in
  let rec encode_nbt nbt =
    match nbt with
    | n, Byte i ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (Byte i)
    | n, Short i ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (Short i)
    | n, Int i ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (Int i)
    | n, Long i ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (Long i)
    | n, String i ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (String i)
    | root, Compound nbt_inner ->
        write_header buf nbt;
        write_tag buf root;
        encode_term (Compound nbt_inner)
    | n, List nbt_inner ->
        write_header buf nbt;
        write_tag buf n;
        encode_term (List nbt_inner)
    | _ -> failwith "Not implemented"
  and encode_term term =
    match term with
    | Byte i -> Buffer.add_char buf (Char.chr i)
    | Short i ->
        Buffer.add_char buf (Char.chr (i lsr 8));
        Buffer.add_char buf (Char.chr i)
    | Int i -> Buffer.add_int32_le buf i
    | Long i -> Buffer.add_int64_le buf i
    | String i ->
        Buffer.add_uint16_le buf (String.length i);
        Buffer.add_string buf i
    | Compound nbt ->
        List.iter encode_nbt nbt;
        Buffer.add_char buf end_tag
    | List terms ->
        Buffer.add_char buf (term_to_tag (List.hd terms));
        Buffer.add_int32_le buf (Int32.of_int (List.length terms));
        List.iter encode_term terms
    | _ -> failwith "Not implemented"
  in
  encode_nbt nbt;
  Buffer.contents buf

let generate_mc_structure_by_size x y z =
  ( "",
    Compound
      [
        ("format_version", Int 1l);
        ( "size",
          List
            [ Int (Int32.of_int x); Int (Int32.of_int y); Int (Int32.of_int z) ]
        );
        ( "structure",
          Compound
            [
              ("block_indices", IntArray (Array.make (x * y * z) 0));
              ( "palette",
                Compound
                  [
                    ( "default",
                      Compound
                        [
                          ( "block_palette",
                            Compound
                              [
                                ("name", String "minecraft:air");
                                ("states", Compound []);
                                ("version", Int 0l);
                              ] );
                        ] );
                  ] );
              ("blocks", ByteArray (Array.make (x * y * z) 0));
              ("entities", List []);
              ("block_entities", List []);
            ] );
        ("structure_world_origin", List [ Int 0l; Int 0l; Int 0l ]);
      ] )
