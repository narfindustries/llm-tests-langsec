module SQLITE3_DB

import Data.ByteString
import Data.Word
import Data.Int
import Data.List

-- SQLite3 Database File Format Specification

-- The SQLite3 database file begins with a 100-byte header.
struct Header {
    magic: ByteString(16) -- "SQLite format 3\000"
    page_size: Word16be
    file_format_write_version: Word8
    file_format_read_version: Word8
    reserved_space: Word8
    max_embedded_payload_fraction: Word8
    min_embedded_payload_fraction: Word8
    leaf_payload_fraction: Word8
    file_change_counter: Word32be
    database_size_in_pages: Word32be
    first_freelist_trunk_page: Word32be
    total_freelist_pages: Word32be
    schema_cookie: Word32be
    schema_format_number: Word32be
    default_page_cache_size: Word32be
    largest_root_btree_page: Word32be
    text_encoding: Word32be
    user_version: Word32be
    incremental_vacuum_mode: Word32be
    application_id: Word32be
    reserved: ByteString(20)
    version_valid_for: Word32be
    sqlite_version_number: Word32be
}

-- A page in the SQLite3 database.
struct Page {
    header: PageHeader
    content: ByteString(header.content_size)
}

-- Page header structure.
struct PageHeader {
    page_type: Word8
    first_freeblock_offset: Word16be
    cell_count: Word16be
    cell_content_area_offset: Word16be
    fragmented_free_bytes: Word8
    right_most_pointer: Maybe Word32be
    cell_pointers: [Word16be]
}

-- The SQLite3 database file.
struct SQLite3DB {
    header: Header
    pages: [Page]
}

-- Helper function to determine the size of the page content.
function PageHeader.content_size(self: PageHeader): Int {
    return self.cell_content_area_offset - 8
}

-- Entry point for parsing the SQLite3 database file.
entrypoint SQLite3DB
