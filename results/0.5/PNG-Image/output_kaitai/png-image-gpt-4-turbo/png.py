# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):
    """PNG (Portable Network Graphics) is an image format that supports lossless compression and varying levels of transparency. This specification covers the critical parts of the PNG format.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.signature = self._io.read_bytes(8)
        if not self.signature == b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A":
            raise kaitaistruct.ValidationNotEqualError(b"\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", self.signature, self._io, u"/seq/0")
        self.chunks = []
        i = 0
        while not self._io.is_eof():
            self.chunks.append(Png.Chunk(self._io, self, self._root))
            i += 1


    class Chunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4le()
            self.type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4le()

        @property
        def is_ihdr(self):
            if hasattr(self, '_m_is_ihdr'):
                return self._m_is_ihdr

            self._m_is_ihdr = self.type == u"IHDR"
            return getattr(self, '_m_is_ihdr', None)

        @property
        def is_plte(self):
            if hasattr(self, '_m_is_plte'):
                return self._m_is_plte

            self._m_is_plte = self.type == u"PLTE"
            return getattr(self, '_m_is_plte', None)

        @property
        def is_idat(self):
            if hasattr(self, '_m_is_idat'):
                return self._m_is_idat

            self._m_is_idat = self.type == u"IDAT"
            return getattr(self, '_m_is_idat', None)

        @property
        def is_iend(self):
            if hasattr(self, '_m_is_iend'):
                return self._m_is_iend

            self._m_is_iend = self.type == u"IEND"
            return getattr(self, '_m_is_iend', None)



