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
        self.header = Sqlite3.Header(self._io, self, self._root)
        self.pages = []
        i = 0
        while not self._io.is_eof():
            self.pages.append(Sqlite3.Page(self._io, self, self._root))
            i += 1


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(18)
            if not self.magic == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00\x30\x30":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00\x30\x30", self.magic, self._io, u"/types/header/seq/0")
            self.page_size = self._io.read_u2be()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_bytes = self._io.read_u1()
            self.max_embedded_payload_frac = self._io.read_u1()
            self.min_embedded_payload_frac = self._io.read_u1()
            self.leaf_payload_frac = self._io.read_u1()
            self.file_change_counter = self._io.read_u4be()
            self.db_size_in_pages = self._io.read_u4be()
            self.first_free_page = self._io.read_u4be()
            self.num_free_pages = self._io.read_u4be()
            self.schema_cookie = self._io.read_u4be()
            self.schema_format_number = self._io.read_u4be()
            self.default_page_cache_size = self._io.read_u4be()
            self.largest_btree_page = self._io.read_u4be()
            self.text_encoding = self._io.read_u4be()
            self.user_version = self._io.read_u4be()
            self.incremental_vacuum_mode = self._io.read_u4be()
            self.application_id = self._io.read_u4be()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4be()
            self.sqlite_version_number = self._io.read_u4be()


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.free_block_offset = self._io.read_u2be()
            self.num_cells = self._io.read_u2be()
            self.first_cell_offset = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()
            if  ((self.page_type == 2) or (self.page_type == 5)) :
                self.right_child_pointer = self._io.read_u4be()

            self.cells = []
            for i in range(self.num_cells):
                self.cells.append(Sqlite3.Cell(self._io, self, self._root))



    class Cell(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_payload = self._io.read_u4be()
            self.rowid = self._io.read_u8be()
            self.payload = self._io.read_bytes(self.len_payload)


    class VlqBase128Le(KaitaiStruct):
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



