# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Tiff(KaitaiStruct):
    """TIFF is a flexible, adaptable file format for handling images and data within a single file, by including the header tags (metadata) defining the image data, such as size, compression applied, color format, etc.
    """
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
            self.byte_order = self._io.read_u2be()
            self.version = self._io.read_u2be()
            self.ifd0_offset = self._io.read_u4be()

        @property
        def is_le(self):
            if hasattr(self, '_m_is_le'):
                return self._m_is_le

            self._m_is_le = self.byte_order == 18761
            return getattr(self, '_m_is_le', None)

        @property
        def is_be(self):
            if hasattr(self, '_m_is_be'):
                return self._m_is_be

            self._m_is_be = self.byte_order == 19789
            return getattr(self, '_m_is_be', None)


    class Ifd(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_entries = self._io.read_u2be()
            self.entries = []
            for i in range(self.num_entries):
                self.entries.append(Tiff.Entry(self._io, self, self._root))

            self.next_ifd_offset = self._io.read_u4be()


    class Entry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u2be()
            self.type = self._io.read_u2be()
            self.count = self._io.read_u4be()
            self.value_offset = self._io.read_u4be()



