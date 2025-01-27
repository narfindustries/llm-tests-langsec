# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """The NITF (National Imagery Transmission Format) format is a standard for formatting digital
    imagery and imagery-related products and exchanging them among members of the intelligence
    and defense communities.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphics_segments = []
        for i in range(self.header.num_graphics_segments):
            self.graphics_segments.append(Nitf.GraphicsSegment(self._io, self, self._root))


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(5)).decode(u"ASCII")
            self.complex_file_length = self._io.read_u4be()
            self.header_length = self._io.read_u2be()
            self.num_image_segments = self._io.read_u2be()
            self.num_graphics_segments = self._io.read_u2be()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.target_id = (self._io.read_bytes(17)).decode(u"ASCII")
            self.image_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.image_data_length = self._io.read_u4be()
            self.image_data = self._io.read_bytes(self.image_data_length)


    class GraphicsSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_id = (self._io.read_bytes(20)).decode(u"ASCII")
            self.graphic_name = (self._io.read_bytes(20)).decode(u"ASCII")
            self.graphic_mode = (self._io.read_bytes(1)).decode(u"ASCII")
            self.graphic_data_length = self._io.read_u4be()
            self.graphic_data = self._io.read_bytes(self.graphic_data_length)



