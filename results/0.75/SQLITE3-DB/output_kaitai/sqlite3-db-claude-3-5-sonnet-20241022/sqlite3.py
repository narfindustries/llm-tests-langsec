# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3(KaitaiStruct):

    class Encoding(Enum):
        utf8 = 1
        utf16le = 2
        utf16be = 3

    class PageType(Enum):
        interior_index = 2
        interior_table = 5
        leaf_index = 10
        leaf_table = 13
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Sqlite3.DatabaseHeader(self._io, self, self._root)
        self.pages = []
        i = 0
        while not self._io.is_eof():
            self.pages.append(Sqlite3.Page(self._io, self, self._root))
            i += 1


    class BtreePageHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.first_freeblock = self._io.read_u2be()
            self.number_of_cells = self._io.read_u2be()
            self.cell_content_area = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()
            if  ((self.page_type == 2) or (self.page_type == 5)) :
                self.rightmost_pointer = self._io.read_u4be()



    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_header = Sqlite3.BtreePageHeader(self._io, self, self._root)
            self.cells = []
            for i in range(self.page_header.number_of_cells):
                self.cells.append(Sqlite3.Cell(self._io, self, self._root))

            self.cell_pointers = []
            for i in range(self.page_header.number_of_cells):
                self.cell_pointers.append(self._io.read_u2be())



    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bytes = []
            i = 0
            while True:
                _ = self._io.read_u1()
                self.bytes.append(_)
                if (_ & 128) == 0:
                    break
                i += 1

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((self.bytes[0] & 127) if len(self.bytes) == 1 else ((((self.bytes[0] & 127) << 7) | (self.bytes[1] & 127)) if len(self.bytes) == 2 else (((((self.bytes[0] & 127) << 14) | ((self.bytes[1] & 127) << 7)) | (self.bytes[2] & 127)) if len(self.bytes) == 3 else ((((((self.bytes[0] & 127) << 21) | ((self.bytes[1] & 127) << 14)) | ((self.bytes[2] & 127) << 7)) | (self.bytes[3] & 127)) if len(self.bytes) == 4 else (((((((self.bytes[0] & 127) << 28) | ((self.bytes[1] & 127) << 21)) | ((self.bytes[2] & 127) << 14)) | ((self.bytes[3] & 127) << 7)) | (self.bytes[4] & 127)) if len(self.bytes) == 5 else ((((((((self.bytes[0] & 127) << 35) | ((self.bytes[1] & 127) << 28)) | ((self.bytes[2] & 127) << 21)) | ((self.bytes[3] & 127) << 14)) | ((self.bytes[4] & 127) << 7)) | (self.bytes[5] & 127)) if len(self.bytes) == 6 else (((((((((self.bytes[0] & 127) << 42) | ((self.bytes[1] & 127) << 35)) | ((self.bytes[2] & 127) << 28)) | ((self.bytes[3] & 127) << 21)) | ((self.bytes[4] & 127) << 14)) | ((self.bytes[5] & 127) << 7)) | (self.bytes[6] & 127)) if len(self.bytes) == 7 else ((((((((((self.bytes[0] & 127) << 49) | ((self.bytes[1] & 127) << 42)) | ((self.bytes[2] & 127) << 35)) | ((self.bytes[3] & 127) << 28)) | ((self.bytes[4] & 127) << 21)) | ((self.bytes[5] & 127) << 14)) | ((self.bytes[6] & 127) << 7)) | (self.bytes[7] & 127)) if len(self.bytes) == 8 else ((((((((((self.bytes[0] & 127) << 56) | ((self.bytes[1] & 127) << 49)) | ((self.bytes[2] & 127) << 42)) | ((self.bytes[3] & 127) << 35)) | ((self.bytes[4] & 127) << 28)) | ((self.bytes[5] & 127) << 21)) | ((self.bytes[6] & 127) << 14)) | ((self.bytes[7] & 127) << 7)) | (self.bytes[8] & 127))))))))))
            return getattr(self, '_m_value', None)


    class Cell(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.payload_size = Sqlite3.Varint(self._io, self, self._root)
            if self._parent.page_header.page_type == 13:
                self.row_id = Sqlite3.Varint(self._io, self, self._root)

            self.payload = self._io.read_bytes(self.payload_size.value)


    class DatabaseHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(16)
            if not self.magic == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.magic, self._io, u"/types/database_header/seq/0")
            self.page_size = self._io.read_u2be()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_space = self._io.read_u1()
            self.max_payload_fraction = self._io.read_u1()
            self.min_payload_fraction = self._io.read_u1()
            self.leaf_payload_fraction = self._io.read_u1()
            self.file_change_counter = self._io.read_u4be()
            self.database_size = self._io.read_u4be()
            self.first_freelist_trunk_page = self._io.read_u4be()
            self.total_freelist_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_root_btree = self._io.read_u4be()
            self.text_encoding = KaitaiStream.resolve_enum(Sqlite3.Encoding, self._io.read_u4be())
            self.user_version = self._io.read_u4be()
            self.incremental_vacuum = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4be()
            self.sqlite_version = self._io.read_u4be()



