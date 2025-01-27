domain sqlite3_db_meta {
  import std.all;

  type db_header = struct {
    magic: uint32,
    page_size: uint16,
    write_version: uint8,
    read_version: uint8,
    reserved: uint8[2],
    max_embedded_payload: uint16,
    min_embedded_payload: uint16,
    leaf_payload_fraction: uint8,
    file_change_counter: uint32,
  };

  type btree_page_header = struct {
    page_type: uint8,
    first_free_block: uint16,
    num_cells: uint16,
    cell_content_area_offset: uint16,
    num_frag_free_bytes: uint8,
    right_child_page: uint32,
    reserved: uint8[4],
  };

  type cell = struct {
    length: uint16,
    type: uint8,
    payload: bytes,
  };

  grammar db_file: db_header, btree_page_header, cell*;
}