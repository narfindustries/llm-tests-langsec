# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """The NITF (National Imagery Transmission Format) format is a file format developed by the U.S. Government for storing imagery and associated metadata. 
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
            self.graphics_segments.append(Nitf.GraphicSegment(self._io, self, self._root))


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes_full()


    class Clasnfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.security_class = self._io.read_bytes(1)
            self.security_system = self._io.read_bytes(2)
            self.codewords = self._io.read_bytes(11)
            self.control_and_handling = self._io.read_bytes(2)
            self.releasing_instructions = self._io.read_bytes(20)
            self.declassification_type = self._io.read_bytes(2)


    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_id = self._io.read_bytes(20)
            self.graphic_name = self._io.read_bytes(20)
            self.graphic_classification = Nitf.Clasnfo(self._io, self, self._root)
            self.encrypted = self._io.read_bits_int_be(1) != 0
            self._io.align_to_byte()
            self.graphic_type = self._io.read_bytes(1)


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_id = self._io.read_bytes(10)
            self.image_date_time = self._io.read_bytes(14)
            self.target_id = self._io.read_bytes(17)
            self.image_title = self._io.read_bytes(80)
            self.image_security = Nitf.Clasnfo(self._io, self, self._root)
            self.encryped = self._io.read_bits_int_be(1) != 0
            self._io.align_to_byte()
            self.image_source = self._io.read_bytes(42)
            self.num_sig_rows = self._io.read_u4be()
            self.num_sig_cols = self._io.read_u4be()
            self.pixel_value_type = self._io.read_bytes(3)
            self.image_representation = self._io.read_bytes(8)
            self.image_category = self._io.read_bytes(8)


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_subheader = Nitf.GraphicSubheader(self._io, self, self._root)
            self.graphic_data = self._io.read_bytes_full()


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = self._io.read_bytes(4)
            self.file_version = self._io.read_bytes(5)
            self.complex_level = self._io.read_u1()
            self.system_type = self._io.read_bytes(4)
            self.origin_station_id = self._io.read_bytes(10)
            self.file_date_time = self._io.read_bytes(14)
            self.file_title = self._io.read_bytes(80)
            self.file_security = Nitf.Clasnfo(self._io, self, self._root)
            self.num_image_segments = self._io.read_u2be()
            self.num_graphics_segments = self._io.read_u2be()



