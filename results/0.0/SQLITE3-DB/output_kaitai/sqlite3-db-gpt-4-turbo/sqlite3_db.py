# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Sqlite3Db(KaitaiStruct):
    """SQLite is a C library that provides a lightweight disk-based database that doesn’t require
    a separate server process and allows accessing the database using a nonstandard variant
    of the SQL query language. Some applications can use SQLite for internal data storage.
    It’s also possible to prototype an application using SQLite and then port the code to a
    larger database such as PostgreSQL or Oracle.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Sqlite3Db.FileHeader(self._io, self, self._root)
        self.pages = []
        i = 0
        while not self._io.is_eof():
            self.pages.append(Sqlite3Db.Page(self._io, self, self._root))
            i += 1


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(16)
            if not self.magic == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.magic, self._io, u"/types/file_header/seq/0")
            self.page_size = self._io.read_u2le()
            self.write_version = self._io.read_u1()
            self.read_version = self._io.read_u1()
            self.reserved_space = self._io.read_u1()
            self.max_payload_frac = self._io.read_u1()
            self.min_payload_frac = self._io.read_u1()
            self.leaf_payload_frac = self._io.read_u1()
            self.file_change_counter = self._io.read_u4le()
            self.num_pages = self._io.read_u4le()
            self.first_freelist_trunk_page = self._io.read_u4le()
            self.num_freelist_pages = self._io.read_u4le()
            self.schema_cookie = self._io.read_u4le()
            self.schema_format = self._io.read_u4le()
            self.default_cache_size = self._io.read_u4le()
            self.largest_root_btree_page = self._io.read_u4le()
            self.text_encoding = self._io.read_u4le()
            self.user_version = self._io.read_u4le()
            self.incremental_vacuum_mode = self._io.read_u4le()
            self.application_id = self._io.read_u4le()
            self.reserved = self._io.read_bytes(20)
            self.version_valid_for = self._io.read_u4le()
            self.sqlite_version_number = self._io.read_u4le()


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
            self.cell_content_area = self._io.read_u2le()
            self.fragmented_free_bytes = self._io.read_u1()
            self.right_most_pointer = self._io.read_u4le()
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
            self.left_child_page = self._io.read_u4le()
            self.rowid = self._io.read_u4le()
            self.payload = self._io.read_bytes(0)



