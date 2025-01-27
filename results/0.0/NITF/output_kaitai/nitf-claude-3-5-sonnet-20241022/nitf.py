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
        self.header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(self.header.num_graphic_segments):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.header.num_text_segments):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_part_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.text_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.text_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.text_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.text_security_classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.text_format = (self._io.read_bytes(3)).decode(u"ASCII")
            self.text_length = self._io.read_u4be()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.subheader.image_length)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subheader = Nitf.TextSubheader(self._io, self, self._root)
            self.text_data = self._io.read_bytes(self.subheader.text_length)


    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_part_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.graphic_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.graphic_name = (self._io.read_bytes(20)).decode(u"ASCII")
            self.graphic_security_classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.graphic_type = (self._io.read_bytes(1)).decode(u"ASCII")
            self.graphic_length = self._io.read_u4be()


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_part_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.image_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.target_id = (self._io.read_bytes(17)).decode(u"ASCII")
            self.image_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.image_security_classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_source = (self._io.read_bytes(42)).decode(u"ASCII")
            self.num_rows = self._io.read_u4be()
            self.num_cols = self._io.read_u4be()
            self.pixel_value_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.image_compression = (self._io.read_bytes(2)).decode(u"ASCII")
            self.image_band_count = self._io.read_u1()
            self.image_length = self._io.read_u4be()


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subheader = Nitf.GraphicSubheader(self._io, self, self._root)
            self.graphic_data = self._io.read_bytes(self.subheader.graphic_length)


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(5)).decode(u"ASCII")
            self.complexity_level = (self._io.read_bytes(2)).decode(u"ASCII")
            self.standard_type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.originating_station_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.file_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.file_security_classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.num_image_segments = self._io.read_u2be()
            self.num_graphic_segments = self._io.read_u2be()
            self.num_text_segments = self._io.read_u2be()
            self.file_length = self._io.read_u4be()



