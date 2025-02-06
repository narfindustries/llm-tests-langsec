# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3(KaitaiStruct):

    class PageTypes(Enum):
        overflow = 0
        freelist = 1
        btree_interior_index = 2
        btree_interior_table = 5
        pointer_map = 7
        btree_leaf_index = 10
        btree_leaf_table = 13

    class Encoding(Enum):
        utf8 = 1
        utf16le = 2
        utf16be = 3
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


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_header = Sqlite3.PageHeader(self._io, self, self._root)
            self.cell_pointers = []
            for i in range(self.page_header.number_of_cells):
                self.cell_pointers.append(self._io.read_u2be())

            self.cells = []
            for i in range(self.page_header.number_of_cells):
                self.cells.append(Sqlite3.Cell(i, self._io, self, self._root))



    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte1 = self._io.read_u1()
            if (self.byte1 & 128) != 0:
                self.byte2 = self._io.read_u1()

            if (self.byte2 & 128) != 0:
                self.byte3 = self._io.read_u1()

            if (self.byte3 & 128) != 0:
                self.byte4 = self._io.read_u1()

            if (self.byte4 & 128) != 0:
                self.byte5 = self._io.read_u1()

            if (self.byte5 & 128) != 0:
                self.byte6 = self._io.read_u1()

            if (self.byte6 & 128) != 0:
                self.byte7 = self._io.read_u1()

            if (self.byte7 & 128) != 0:
                self.byte8 = self._io.read_u1()

            if (self.byte8 & 128) != 0:
                self.byte9 = self._io.read_u1()


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (((((((((self.byte1 & 127) | ((self.byte2 & 127) << 7)) | ((self.byte3 & 127) << 14)) | ((self.byte4 & 127) << 21)) | ((self.byte5 & 127) << 28)) | ((self.byte6 & 127) << 35)) | ((self.byte7 & 127) << 42)) | ((self.byte8 & 127) << 49)) | ((self.byte9 & 127) << 56))
            return getattr(self, '_m_value', None)


    class PageHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.first_freeblock = self._io.read_u2be()
            self.number_of_cells = self._io.read_u2be()
            self.cell_content_offset = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()
            if  ((self.page_type == 2) or (self.page_type == 5)) :
                self.right_child_pointer = self._io.read_u4be()



    class Cell(KaitaiStruct):
        def __init__(self, cell_index, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self.cell_index = cell_index
            self._read()

        def _read(self):
            self.payload_length = Sqlite3.Varint(self._io, self, self._root)
            if self._parent.page_header.page_type == 13:
                self.row_id = Sqlite3.Varint(self._io, self, self._root)

            self.payload = self._io.read_bytes(self.payload_length.value)
            if self.payload_length.value > (0 if self._parent.page_header.page_type == 13 else self.payload_length.value):
                self.overflow_page = self._io.read_u4be()



    class DatabaseHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_string = self._io.read_bytes(16)
            if not self.header_string == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.header_string, self._io, u"/types/database_header/seq/0")
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
            self.freelist_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_root_btree_page = self._io.read_u4be()
            self.text_encoding = self._io.read_u4be()
            self.user_version = self._io.read_u4be()
            self.vacuum_mode = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4be()
            self.sqlite_version = self._io.read_u4be()



