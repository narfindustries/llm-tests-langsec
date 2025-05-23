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
        self.magic = self._io.read_bytes(16)
        if not self.magic == b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00":
            raise kaitaistruct.ValidationNotEqualError(b"\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", self.magic, self._io, u"/seq/0")
        self.page_size = self._io.read_u2be()
        self.file_format_write_version = self._io.read_u1()
        self.file_format_read_version = self._io.read_u1()
        self.reserved_space = self._io.read_u1()
        self.max_embedded_payload_fraction = self._io.read_u1()
        self.min_embedded_payload_fraction = self._io.read_u1()
        self.leaf_payload_fraction = self._io.read_u1()
        self.file_change_counter = self._io.read_u4be()
        self.db_size_in_pages = self._io.read_u4be()
        self.first_freelist_trunk_page = self._io.read_u4be()
        self.total_freelist_pages = self._io.read_u4be()
        self.schema_cookie = self._io.read_u4be()
        self.schema_format_number = self._io.read_u4be()
        self.default_page_cache_size = self._io.read_u4be()
        self.largest_root_btree_page = self._io.read_u4be()
        self.text_encoding = self._io.read_u4be()
        self.user_version = self._io.read_s4be()
        self.incremental_vacuum_mode = self._io.read_u4be()
        self.application_id = self._io.read_s4be()
        self.reserved_for_expansion = self._io.read_u4be()
        self.version_valid_for_number = self._io.read_u4be()
        self.sqlite_version_number = self._io.read_u4be()

    class BtreePage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.first_freeblock_offset = self._io.read_u2be()
            self.number_of_cells = self._io.read_u2be()
            self.offset_to_cell_content_area = self._io.read_u2be()
            self.fragmented_free_bytes = self._io.read_u1()


    class FreelistPage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.next_freelist_trunk_page = self._io.read_u4be()
            self.number_of_free_pages = self._io.read_u4be()


    class PointerMapPage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.page_type = self._io.read_u1()
            self.parent_page_number = self._io.read_u4be()


    class WalHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic_number = self._io.read_u4be()
            self.file_format_version = self._io.read_u4be()
            self.page_size = self._io.read_u4be()
            self.checkpoint_sequence_number = self._io.read_u4be()
            self.salt_1 = self._io.read_u4be()
            self.salt_2 = self._io.read_u4be()
            self.checksum_1 = self._io.read_u4be()
            self.checksum_2 = self._io.read_u4be()



