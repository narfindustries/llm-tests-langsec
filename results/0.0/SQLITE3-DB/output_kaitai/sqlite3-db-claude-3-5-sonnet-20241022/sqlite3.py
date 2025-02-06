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
        overflow_or_freelist = 0
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


    class BlobOrText(KaitaiStruct):
        def __init__(self, type_code, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self.type_code = type_code
            self._read()

        def _read(self):
            self.value = self._io.read_bytes((self.type_code - 12) // 2)


    class ConstZero(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class Bytes3(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bytes(3)


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = KaitaiStream.resolve_enum(Sqlite3.PageType, self._io.read_u1())
            self.first_freeblock = self._io.read_u2be()
            self.num_cells = self._io.read_u2be()
            self.cell_content_area = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()
            if  ((self.page_type == Sqlite3.PageType.interior_index) or (self.page_type == Sqlite3.PageType.interior_table)) :
                self.right_child_pointer = self._io.read_u4be()

            self.cells = []
            for i in range(self.num_cells):
                self.cells.append(Sqlite3.Cell(self._io, self, self._root))



    class ConstOne(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class Bytes6(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bytes(6)


    class ColumnValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            _on = self._parent.column_types[i]
            if _on == 0:
                self.value = Sqlite3.NullType(self._io, self, self._root)
            elif _on == 4:
                self.value = self._io.read_u4be()
            elif _on == 6:
                self.value = self._io.read_u8be()
            elif _on == 7:
                self.value = self._io.read_f8be()
            elif _on == 1:
                self.value = self._io.read_u1()
            elif _on == 3:
                self.value = Sqlite3.Bytes3(self._io, self, self._root)
            elif _on == 5:
                self.value = Sqlite3.Bytes6(self._io, self, self._root)
            elif _on == 8:
                self.value = Sqlite3.ConstZero(self._io, self, self._root)
            elif _on == 9:
                self.value = Sqlite3.ConstOne(self._io, self, self._root)
            elif _on == 2:
                self.value = self._io.read_u2be()
            else:
                self.value = Sqlite3.BlobOrText(self._parent.column_types[i], self._io, self, self._root)


    class Cell(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cell_type = self._io.read_u1()
            self.len_payload = self._io.read_u4be()
            if self._parent.page_type == Sqlite3.PageType.leaf_table:
                self.row_id = self._io.read_u4be()

            if  ((self._parent.page_type == Sqlite3.PageType.leaf_table) or (self._parent.page_type == Sqlite3.PageType.leaf_index)) :
                self._raw_payload = self._io.read_bytes(self.len_payload)
                _io__raw_payload = KaitaiStream(BytesIO(self._raw_payload))
                self.payload = Sqlite3.Record(_io__raw_payload, self, self._root)

            if  ((self._parent.page_type == Sqlite3.PageType.interior_table) or (self._parent.page_type == Sqlite3.PageType.interior_index)) :
                self.page_number = self._io.read_u4be()

            if self._parent.page_type == Sqlite3.PageType.interior_table:
                self.key = self._io.read_u4be()



    class NullType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


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
            self.num_freelist_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_root_btree_page = self._io.read_u4be()
            self.text_encoding = KaitaiStream.resolve_enum(Sqlite3.Encoding, self._io.read_u4be())
            self.user_version = self._io.read_u4be()
            self.vacuum_mode = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4be()
            self.sqlite_version = self._io.read_u4be()


    class Record(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_size = self._io.read_u4be()
            self.column_types = []
            for i in range(self.header_size):
                self.column_types.append(self._io.read_u1())

            self.values = []
            for i in range(self.header_size):
                self.values.append(Sqlite3.ColumnValue(self._io, self, self._root))




