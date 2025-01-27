# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Nitf.NitfHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.header.num_text_segments):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.header.num_data_extension_segments):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))


    class NitfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(5)).decode(u"ASCII")
            self.num_image_segments = self._io.read_u1()
            self.num_text_segments = self._io.read_u1()
            self.num_data_extension_segments = self._io.read_u1()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_length = self._io.read_u2be()
            self.image_data = self._io.read_bytes_full()


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_length = self._io.read_u2be()
            self.text_data = self._io.read_bytes_full()


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_length = self._io.read_u2be()
            self.data = self._io.read_bytes_full()



