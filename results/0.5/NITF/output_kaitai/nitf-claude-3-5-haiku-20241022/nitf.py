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
        self.file_header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.file_header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(self.file_header.num_graphic_segments):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.file_header.num_text_segments):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.file_header.num_data_extension_segments):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.file_header.num_reserved_extension_segments):
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))


    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved_extension_subheader = Nitf.ReservedExtensionSubheader(self._io, self, self._root)
            self.reserved_extension_data = self._io.read_bytes_full()


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_identifier = (self._io.read_bytes(10)).decode(u"ASCII")
            self.text_datetime = (self._io.read_bytes(14)).decode(u"ASCII")
            self.text_title = (self._io.read_bytes(80)).decode(u"ASCII")


    class ReservedExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.re_identifier = (self._io.read_bytes(2)).decode(u"ASCII")


    class DataExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.de_identifier = (self._io.read_bytes(2)).decode(u"ASCII")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes_full()


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader = Nitf.TextSubheader(self._io, self, self._root)
            self.text_data = self._io.read_bytes_full()


    class ImageGeolocation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.coords = []
            for i in range(4):
                self.coords.append(Nitf.Coordinate(self._io, self, self._root))



    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_identifier = (self._io.read_bytes(10)).decode(u"ASCII")
            self.graphic_name = (self._io.read_bytes(20)).decode(u"ASCII")


    class SecurityClassification(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.country_code = (self._io.read_bytes(2)).decode(u"ASCII")
            self.release_instructions = (self._io.read_bytes(20)).decode(u"ASCII")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data_extension_subheader = Nitf.DataExtensionSubheader(self._io, self, self._root)
            self.data_extension_data = self._io.read_bytes_full()


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4be()
            self.image_identifier = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_datetime = (self._io.read_bytes(14)).decode(u"ASCII")
            self.image_target_identifier = (self._io.read_bytes(17)).decode(u"ASCII")
            self.image_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.security_classification = Nitf.SecurityClassification(self._io, self, self._root)
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_source = (self._io.read_bytes(42)).decode(u"ASCII")
            self.significant_rows = self._io.read_u4be()
            self.significant_columns = self._io.read_u4be()
            self.pixel_value_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.image_representation = (self._io.read_bytes(8)).decode(u"ASCII")
            self.image_category = (self._io.read_bytes(3)).decode(u"ASCII")
            self.actual_bits_per_pixel = self._io.read_u2be()
            self.pixel_justification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_coordinate_representation = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_geolocation = Nitf.ImageGeolocation(self._io, self, self._root)


    class Coordinate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.latitude = self._io.read_f8be()
            self.longitude = self._io.read_f8be()


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
            self.file_profile_name = (self._io.read_bytes(9)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(4)).decode(u"ASCII")
            self.complexity_level = (self._io.read_bytes(2)).decode(u"ASCII")
            self.standard_type = (self._io.read_bytes(1)).decode(u"ASCII")
            self.originating_station_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.file_datetime = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.security_classification = Nitf.SecurityClassification(self._io, self, self._root)
            self.security_system = (self._io.read_bytes(2)).decode(u"ASCII")
            self.security_code = (self._io.read_bytes(11)).decode(u"ASCII")
            self.security_control_and_release_markings = (self._io.read_bytes(20)).decode(u"ASCII")
            self.security_downgrade = (self._io.read_bytes(6)).decode(u"ASCII")
            self.security_downgrade_datetime = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_copy_number = (self._io.read_bytes(5)).decode(u"ASCII")
            self.file_number_of_copies = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.num_image_segments = self._io.read_u2be()
            self.num_graphic_segments = self._io.read_u2be()
            self.num_text_segments = self._io.read_u2be()
            self.num_data_extension_segments = self._io.read_u2be()
            self.num_reserved_extension_segments = self._io.read_u2be()
            self.user_defined_header_length = self._io.read_u2be()
            self.user_defined_header = (self._io.read_bytes(self.user_defined_header_length)).decode(u"ASCII")
            self.extended_header_length = self._io.read_u2be()
            self.extended_header = (self._io.read_bytes(self.extended_header_length)).decode(u"ASCII")



