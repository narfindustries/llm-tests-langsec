# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gzip(KaitaiStruct):

    class CompressionMethod(Enum):
        reserved0 = 0
        reserved1 = 1
        reserved2 = 2
        reserved3 = 3
        reserved4 = 4
        reserved5 = 5
        reserved6 = 6
        reserved7 = 7
        deflate = 8
        reserved9 = 9
        reserved10 = 10
        reserved11 = 11
        reserved12 = 12
        reserved13 = 13
        reserved14 = 14
        reserved15 = 15

    class ExtraFlags(Enum):
        maximum_compression = 2
        fastest_compression = 4

    class OperatingSystem(Enum):
        fat = 0
        amiga = 1
        vms = 2
        unix = 3
        vm_cms = 4
        atari_tos = 5
        hpfs = 6
        macintosh = 7
        z_system = 8
        cp_m = 9
        tops_20 = 10
        ntfs = 11
        qdos = 12
        acorn_riscos = 13
        unknown = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic1 = self._io.read_bytes(1)
        if not self.magic1 == b"\x1F":
            raise kaitaistruct.ValidationNotEqualError(b"\x1F", self.magic1, self._io, u"/seq/0")
        self.magic2 = self._io.read_bytes(1)
        if not self.magic2 == b"\x8B":
            raise kaitaistruct.ValidationNotEqualError(b"\x8B", self.magic2, self._io, u"/seq/1")
        self.compression_method = KaitaiStream.resolve_enum(Gzip.CompressionMethod, self._io.read_u1())
        self.flags = Gzip.Flags(self._io, self, self._root)
        self.mtime = self._io.read_u4le()
        self.extra_flags = KaitaiStream.resolve_enum(Gzip.ExtraFlags, self._io.read_u1())
        self.operating_system = KaitaiStream.resolve_enum(Gzip.OperatingSystem, self._io.read_u1())
        if (self.flags.flag_byte & 4) != 0:
            self.extra_field = Gzip.ExtraField(self._io, self, self._root)

        if (self.flags.flag_byte & 8) != 0:
            self.name = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

        if (self.flags.flag_byte & 16) != 0:
            self.comment = (self._io.read_bytes_term(0, False, True, True)).decode(u"utf-8")

        if (self.flags.flag_byte & 2) != 0:
            self.header_crc16 = self._io.read_u2le()

        self.compressed_data = self._io.read_bytes_full()
        self.crc32 = self._io.read_u4le()
        self.isize = self._io.read_u4le()

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flag_byte = self._io.read_u1()

        @property
        def reserved2(self):
            if hasattr(self, '_m_reserved2'):
                return self._m_reserved2

            self._m_reserved2 = ((self.flag_byte >> 6) & 1)
            return getattr(self, '_m_reserved2', None)

        @property
        def reserved1(self):
            if hasattr(self, '_m_reserved1'):
                return self._m_reserved1

            self._m_reserved1 = ((self.flag_byte >> 5) & 1)
            return getattr(self, '_m_reserved1', None)

        @property
        def flag_text(self):
            if hasattr(self, '_m_flag_text'):
                return self._m_flag_text

            self._m_flag_text = (self.flag_byte & 1)
            return getattr(self, '_m_flag_text', None)

        @property
        def flag_extra(self):
            if hasattr(self, '_m_flag_extra'):
                return self._m_flag_extra

            self._m_flag_extra = ((self.flag_byte >> 2) & 1)
            return getattr(self, '_m_flag_extra', None)

        @property
        def reserved3(self):
            if hasattr(self, '_m_reserved3'):
                return self._m_reserved3

            self._m_reserved3 = ((self.flag_byte >> 7) & 1)
            return getattr(self, '_m_reserved3', None)

        @property
        def flag_name(self):
            if hasattr(self, '_m_flag_name'):
                return self._m_flag_name

            self._m_flag_name = ((self.flag_byte >> 3) & 1)
            return getattr(self, '_m_flag_name', None)

        @property
        def flag_comment(self):
            if hasattr(self, '_m_flag_comment'):
                return self._m_flag_comment

            self._m_flag_comment = ((self.flag_byte >> 4) & 1)
            return getattr(self, '_m_flag_comment', None)

        @property
        def flag_hcrc(self):
            if hasattr(self, '_m_flag_hcrc'):
                return self._m_flag_hcrc

            self._m_flag_hcrc = ((self.flag_byte >> 1) & 1)
            return getattr(self, '_m_flag_hcrc', None)


    class ExtraField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_extra_data = self._io.read_u2le()
            self.extra_data = self._io.read_bytes(self.len_extra_data)



