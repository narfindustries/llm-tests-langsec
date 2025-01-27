# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):

    class FieldType(Enum):
        byte = 1
        ascii = 2
        short = 3
        long = 4
        rational = 5
        sbyte = 6
        undefined = 7
        sshort = 8
        slong = 9
        srational = 10
        float = 11
        double = 12
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Tiff.Header(self._io, self, self._root)

    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_order = self._io.read_bytes(4)
            if not self.byte_order == b"\x49\x49\x4D\x4D":
                raise kaitaistruct.ValidationNotEqualError(b"\x49\x49\x4D\x4D", self.byte_order, self._io, u"/types/header/seq/0")
            self.version = self._io.read_bytes(1)
            if not self.version == b"\x2A":
                raise kaitaistruct.ValidationNotEqualError(b"\x2A", self.version, self._io, u"/types/header/seq/1")
            self.ifd_offset = self._io.read_u4le()


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_entries = self._io.read_u2le()
            self.entries = []
            for i in range(self.num_entries):
                self.entries.append(Tiff.IfdEntry(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4le()


    class IfdEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u2le()
            self.field_type = self._io.read_u2le()
            self.num_values = self._io.read_u4le()
            self.value_offset = self._io.read_u4le()


    @property
    def ifds(self):
        if hasattr(self, '_m_ifds'):
            return self._m_ifds

        _pos = self._io.pos()
        self._io.seek(self.header.ifd_offset)
        self._raw__m_ifds = []
        self._m_ifds = []
        i = 0
        while True:
            _buf = self._io.read_bytes_full()
            self._raw__m_ifds.append(_buf)
            _io__raw__m_ifds = KaitaiStream(BytesIO(self._raw__m_ifds[-1]))
            _ = Tiff.Ifd(_io__raw__m_ifds, self, self._root)
            self._m_ifds.append(_)
            if _.next_ifd_offset == 0:
                break
            i += 1
        self._io.seek(_pos)
        return getattr(self, '_m_ifds', None)


