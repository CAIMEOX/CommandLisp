open Nbt

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

(* The function trans system into mcs should generate the chain properly *)
