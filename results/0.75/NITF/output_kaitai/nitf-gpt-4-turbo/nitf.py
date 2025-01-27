# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """The National Imagery Transmission Format (NITF) is a standard for formatting digital
    imagery and imagery-related products and exchanging them among members of the
    Intelligence Community (IC) as defined by the United States.
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

        self.text_segments = []
        for i in range(self.header.num_text_segments):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_sub_header = Nitf.ImageSubHeader(self._io, self, self._root)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_sub_header = Nitf.TextSubHeader(self._io, self, self._root)


    class GraphicSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_id = (self._io.read_bytes(20)).decode(u"ASCII")


    class GraphicsSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_sub_header = Nitf.GraphicSubHeader(self._io, self, self._root)


    class ImageSubHeader(KaitaiStruct):
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


    class TextSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_id = (self._io.read_bytes(7)).decode(u"ASCII")
            self.text_date_time = (self._io.read_bytes(14)).decode(u"ASCII")


    class SecurityMarking(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.classification_system = (self._io.read_bytes(2)).decode(u"ASCII")
            self.codewords = (self._io.read_bytes(11)).decode(u"ASCII")
            self.control_and_handling = (self._io.read_bytes(2)).decode(u"ASCII")
            self.releaseability = (self._io.read_bytes(20)).decode(u"ASCII")
            self.declass_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.declass_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.declass_exemption = (self._io.read_bytes(4)).decode(u"ASCII")
            self.downgrade = (self._io.read_bytes(1)).decode(u"ASCII")
            self.downgrade_date = (self._io.read_bytes(8)).decode(u"ASCII")


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
            self.system_type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.origin_station_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.file_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.file_security = Nitf.SecurityMarking(self._io, self, self._root)
            self.num_image_segments = self._io.read_u2be()
            self.num_graphics_segments = self._io.read_u2be()
            self.num_text_segments = self._io.read_u2be()



