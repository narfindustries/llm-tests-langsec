format sqlite3_db {
  magic: bytes >= 4 <= 4;
  file_format_write_version: byte;
  file_format_read_version: byte;
  reserved: bytes >= 10 <= 10;
  max_page_size: int >= 2 <= 4;
  def_page_cache_size: int >= 2 <= 4;
  text_encoding: byte;
  user_version: int >= 2 <= 4;
  incremental_vacuum_mode: byte;
  application_specific: bytes >= 3 <= 3;
  version_valid_num: int >= 2 <= 4;
  sqlite_version: int >= 2 <= 4;
  pages: page[];
}

format page {
  page_type: byte;
  first_freeblock: int >= 2 <= 2;
  cell_offset: int >= 2 <= 2;
  num_cells: int >= 2 <= 2;
  cell_content_offset: byte >= 0 <= 255;
  cells: cell[];
}

format cell {
  payload_length: int >= 2 <= 2;
  header_length: byte >= 0 <= 255;
  num_cols: byte >= 0 <= 255;
  columns: column[];
}

format column {
  column_data_type: byte >= 0 <= 255;
  column_data: bytes;
}

format btree_page {
  page_num: int >= 2 <= 4;
  right_child_page_num: int >= 2 <= 4;
  num_keys: int >= 2 <= 4;
  keys: key[];
}

format key {
  key: bytes;
}

format sqlite_master {
  type: byte;
  name: bytes;
  tbl_name: bytes;
  rootpage: int >= 2 <= 4;
  sql: bytes;
}

format sqlite_sequence {
  name: bytes;
  seq: int >= 2 <= 4;
}

enum page_type {
  leaf_index_page = 1;
  internal_index_page = 2;
  table_leaf_page = 3;
  table_internal_page = 4;
  index_page = 5;
  overflow_page = 6;
}