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
        self.file_header = Nitf.FileHeaderType(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.file_header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegmentType(self._io, self, self._root))


    class FileHeaderType(KaitaiStruct):
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
            self.num_image_segments = self._io.read_u4be()


    class ImageSegmentType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.ImageSubheaderType(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.image_subheader.image_length)


    class ImageSubheaderType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_identifier = (self._io.read_bytes(2)).decode(u"ASCII")
            self.image_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.target_identifier = (self._io.read_bytes(17)).decode(u"ASCII")
            self.image_identifier2 = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_security_classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_source = (self._io.read_bytes(42)).decode(u"ASCII")
            self.num_significant_rows = self._io.read_u4be()
            self.num_significant_cols = self._io.read_u4be()
            self.pixel_value_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.image_representation = (self._io.read_bytes(8)).decode(u"ASCII")
            self.image_category = (self._io.read_bytes(8)).decode(u"ASCII")
            self.actual_bits_per_pixel = self._io.read_u1()
            self.pixel_justification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_coordinate_system = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_compression = (self._io.read_bytes(2)).decode(u"ASCII")
            self.compression_rate_code = (self._io.read_bytes(4)).decode(u"ASCII")
            self.image_length = self._io.read_u4be()



