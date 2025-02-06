# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Sqlite3.DatabaseHeader(self._io, self, self._root)
        self.pages = []
        for i in range(self.header.database_size):
            self.pages.append(Sqlite3.Page(self._io, self, self._root))


    class BtreePageHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.first_freeblock = self._io.read_u2be()
            self.cell_count = self._io.read_u2be()
            self.cell_content_offset = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            _on = self.page_type
            if _on == 10:
                self.body = Sqlite3.BtreePageLeafIndex(self._io, self, self._root)
            elif _on == 0:
                self.body = Sqlite3.OverflowOrFreelistPage(self._io, self, self._root)
            elif _on == 13:
                self.body = Sqlite3.BtreePageLeafTable(self._io, self, self._root)
            elif _on == 5:
                self.body = Sqlite3.BtreePageInteriorTable(self._io, self, self._root)
            elif _on == 2:
                self.body = Sqlite3.BtreePageInteriorIndex(self._io, self, self._root)


    class BtreeInteriorCellIndex(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.left_child_page = self._io.read_u4be()
            self.len_payload = self._io.read_u4be()
            self.payload = self._io.read_bytes(self.len_payload)


    class BtreePageInteriorTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Sqlite3.BtreePageHeader(self._io, self, self._root)
            self.rightmost_pointer = self._io.read_u4be()
            self.cells = []
            for i in range(self.header.cell_count):
                self.cells.append(Sqlite3.BtreeInteriorCellTable(self._io, self, self._root))



    class BtreePageLeafTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Sqlite3.BtreePageHeader(self._io, self, self._root)
            self.cells = []
            for i in range(self.header.cell_count):
                self.cells.append(Sqlite3.BtreeLeafCellTable(self._io, self, self._root))



    class BtreePageLeafIndex(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Sqlite3.BtreePageHeader(self._io, self, self._root)
            self.cells = []
            for i in range(self.header.cell_count):
                self.cells.append(Sqlite3.BtreeLeafCellIndex(self._io, self, self._root))



    class OverflowOrFreelistPage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.next_trunk_page = self._io.read_u4be()
            self.num_leaf_pages = self._io.read_u4be()
            self.leaf_pages = []
            for i in range(self.num_leaf_pages):
                self.leaf_pages.append(self._io.read_u4be())



    class BtreeLeafCellTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_payload = self._io.read_u4be()
            self.row_id = self._io.read_u4be()
            self.payload = self._io.read_bytes(self.len_payload)


    class BtreeInteriorCellTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.left_child_page = self._io.read_u4be()
            self.row_id = self._io.read_u4be()


    class BtreePageInteriorIndex(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Sqlite3.BtreePageHeader(self._io, self, self._root)
            self.rightmost_pointer = self._io.read_u4be()
            self.cells = []
            for i in range(self.header.cell_count):
                self.cells.append(Sqlite3.BtreeInteriorCellIndex(self._io, self, self._root))



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
            self.text_encoding = self._io.read_u4be()
            self.user_version = self._io.read_u4be()
            self.incremental_vacuum = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4be()
            self.sqlite_version = self._io.read_u4be()


    class BtreeLeafCellIndex(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_payload = self._io.read_u4be()
            self.payload = self._io.read_bytes(self.len_payload)



