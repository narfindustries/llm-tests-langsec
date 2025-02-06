# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3Db(KaitaiStruct):

    class PageType(Enum):
        interior_index = 2
        interior_table = 5
        leaf_index = 10
        leaf_table = 13

    class TextEncoding(Enum):
        utf8 = 1
        utf16le = 2
        utf16be = 3
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Sqlite3Db.Header(self._io, self, self._root)

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic_header_string = self._io.read_bytes(16)
            if not self.magic_header_string == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.magic_header_string, self._io, u"/types/header/seq/0")
            self.page_size = self._io.read_u2be()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_space = self._io.read_u1()
            self.max_payload_fraction = self._io.read_u1()
            self.min_payload_fraction = self._io.read_u1()
            self.leaf_payload_fraction = self._io.read_u1()
            self.file_change_counter = self._io.read_u4be()
            self.database_size_pages = self._io.read_u4be()
            self.first_freelist_trunk_page = self._io.read_u4be()
            self.total_freelist_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format_number = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_root_btree_page = self._io.read_u4be()
            self.text_encoding = KaitaiStream.resolve_enum(Sqlite3Db.TextEncoding, self._io.read_u4be())
            self.user_version = self._io.read_u4be()
            self.incremental_vacuum_mode = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_number = self._io.read_u4be()
            self.sqlite_version_number = self._io.read_u4be()


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = KaitaiStream.resolve_enum(Sqlite3Db.PageType, self._io.read_u1())
            self.first_freeblock_offset = self._io.read_u2be()
            self.cells_count = self._io.read_u2be()
            self.cell_content_area_start = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()
            if  ((self.page_type == Sqlite3Db.PageType.interior_index) or (self.page_type == Sqlite3Db.PageType.interior_table)) :
                self.rightmost_pointer = self._io.read_u4be()



    @property
    def first_page(self):
        if hasattr(self, '_m_first_page'):
            return self._m_first_page

        _pos = self._io.pos()
        self._io.seek(100)
        self._m_first_page = Sqlite3Db.Page(self._io, self, self._root)
        self._io.seek(_pos)
        return getattr(self, '_m_first_page', None)


