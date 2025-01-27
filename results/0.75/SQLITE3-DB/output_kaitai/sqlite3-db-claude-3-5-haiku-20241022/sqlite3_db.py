# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3Db(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Sqlite3Db.Header(self._io, self, self._root)
        self.pages = []
        i = 0
        while not self._io.is_eof():
            self.pages.append(Sqlite3Db.Page(self._io, self, self._root))
            i += 1


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic_string = self._io.read_bytes(16)
            if not self.magic_string == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.magic_string, self._io, u"/types/header/seq/0")
            self.page_size = self._io.read_u2le()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_space = self._io.read_bytes(20)


    class Page(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.first_freeblock = self._io.read_u2le()
            self.num_cells = self._io.read_u2le()
            self.cell_content_start = self._io.read_u2le()
            self.fragmented_free_bytes = self._io.read_u1()
            self.cells = []
            for i in range(self.num_cells):
                self.cells.append(Sqlite3Db.Cell(self._io, self, self._root))



    class Cell(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.payload_size = Sqlite3Db.VlqBase128Le(self._io, self, self._root)
            self.row_id = Sqlite3Db.VlqBase128Le(self._io, self, self._root)
            self.payload = self._io.read_bytes(self.payload_size.value)


    class VlqBase128Le(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups = []
            i = 0
            while True:
                _ = Sqlite3Db.VlqBase128Le.VlqGroup(self._io, self, self._root)
                self.groups.append(_)
                if not (_.has_next):
                    break
                i += 1

        class VlqGroup(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.has_next = self._io.read_bits_int_be(1) != 0
                self.value = self._io.read_bits_int_be(7)


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((((self.groups[0].value & 127) + (((self.groups[1].value & 127) << 7) if len(self.groups) > 1 else 0)) + (((self.groups[2].value & 127) << 14) if len(self.groups) > 2 else 0)) + (((self.groups[3].value & 127) << 21) if len(self.groups) > 3 else 0))
            return getattr(self, '_m_value', None)



