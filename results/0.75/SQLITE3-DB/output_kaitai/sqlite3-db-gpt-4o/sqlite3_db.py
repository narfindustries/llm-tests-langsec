# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3Db(KaitaiStruct):

    class TextEncoding(Enum):
        utf_8 = 1
        utf_16le = 2
        utf_16be = 3
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
            self.header_string = (self._io.read_bytes(16)).decode(u"UTF-8")
            self.page_size = self._io.read_u2be()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_space = self._io.read_u1()
            self.max_embedded_payload_fraction = self._io.read_u1()
            self.min_embedded_payload_fraction = self._io.read_u1()
            self.leaf_payload_fraction = self._io.read_u1()
            self.file_change_counter = self._io.read_u4be()
            self.database_size_in_pages = self._io.read_u4be()
            self.first_freelist_trunk_page = self._io.read_u4be()
            self.total_freelist_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format_number = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_btree_page_number = self._io.read_u4be()
            self.text_encoding = self._io.read_u4be()
            self.user_version = self._io.read_u4be()
            self.incremental_vacuum_mode = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved_for_expansion = self._io.read_bytes(20)
            self.version_valid_for_number = self._io.read_u4be()
            self.sqlite_version_number = self._io.read_u4be()


    @property
    def is_valid_header(self):
        if hasattr(self, '_m_is_valid_header'):
            return self._m_is_valid_header

        self._m_is_valid_header = self.header.header_string == u"SQLite format 3\000"
        return getattr(self, '_m_is_valid_header', None)


