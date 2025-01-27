module SQLITE3_DB {
  import DAEDALUS::Core;

  type SQLite3Header = struct {
    magic       : array[16] of uint8;  // The SQLite file header string: "SQLite format 3\000"
    page_size   : uint16;              // The database page size in bytes.
    file_format_write_version : uint8; // File format write version
    file_format_read_version  : uint8; // File format read version
    reserved_space : uint8;            // Bytes of unused "reserved" space at the end of each page
    max_payload_frac : uint8;          // Maximum payload fraction per page
    min_payload_frac : uint8;          // Minimum payload fraction per page
    leaf_payload_frac : uint8;         // Leaf payload fraction per page
    file_change_counter : uint32;      // File change counter
    num_pages : uint32;                // Number of pages in the database file
    first_freelist_trunk_page : uint32; // First freelist trunk page
    num_freelist_pages : uint32;       // Number of freelist pages
    schema_cookie : uint32;            // Schema cookie
    schema_format : uint32;            // Schema format number
    default_cache_size : int32;        // Default page cache size
    largest_root_btree_page : uint32;  // Largest B-tree page number
    text_encoding : uint32;            // Text encoding used
    user_version : uint32;             // User version
    incremental_vacuum_mode : uint32;  // Incremental vacuum mode
    application_id : uint32;           // Application ID
    reserved : array[20] of uint8;     // Reserved for expansion
    version_valid_for : uint32;        // Version valid for number
    sqlite_version_number : uint32;    // SQLite version number
  };

  type SQLite3Database = struct {
    header : SQLite3Header;
    pages : array[num_pages] of SQLite3Page;
  };

  type SQLite3Page = struct {
    data : array[page_size] of uint8;
  };

  let main = SQLite3Database;
}