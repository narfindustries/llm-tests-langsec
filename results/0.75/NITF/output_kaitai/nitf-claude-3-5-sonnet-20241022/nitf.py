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
        for i in range(int(self.header.num_image_segments)):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.file_version = (self._io.read_bytes(5)).decode(u"UTF-8")
            self.complexity_level = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.standard_type = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.originating_station_id = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.date_time = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.file_title = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.file_security_classification = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.num_image_segments = (self._io.read_bytes(3)).decode(u"UTF-8")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(int(self.subheader.image_length))


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_part_type = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.image_id = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.date_time = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.target_id = (self._io.read_bytes(17)).decode(u"UTF-8")
            self.image_title = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.image_security_classification = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.encryption = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.image_source = (self._io.read_bytes(42)).decode(u"UTF-8")
            self.num_significant_rows = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.num_significant_cols = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.pixel_value_type = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.image_representation = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.image_category = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.actual_bits_per_pixel = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.pixel_justification = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.image_coordinate_system = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.image_compression = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.compression_rate_code = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.image_length = (self._io.read_bytes(10)).decode(u"UTF-8")



