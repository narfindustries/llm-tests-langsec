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
        i = 0
        while not self._io.is_eof():
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))
            i += 1

        self.text_segments = []
        i = 0
        while not self._io.is_eof():
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))
            i += 1

        self.data_extension_segments = []
        i = 0
        while not self._io.is_eof():
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))
            i += 1

        self.reserved_extension_segments = []
        i = 0
        while not self._io.is_eof():
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))
            i += 1


    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved_extension_subheader = Nitf.ReservedExtensionSubheader(self._io, self, self._root)
            self.reserved_extension_data = (self._io.read_bytes_full()).decode(u"ASCII")


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.text_date_and_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.text_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.text_security = Nitf.FileSecurity(self._io, self, self._root)
            self.text_format = (self._io.read_bytes(3)).decode(u"ASCII")
            self.text_extension = (self._io.read_bytes(3)).decode(u"ASCII")


    class ImageBand(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.band_id = (self._io.read_bytes(2)).decode(u"ASCII")
            self.band_representation = (self._io.read_bytes(2)).decode(u"ASCII")
            self.band_data_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.band_number_of_blocks = self._io.read_u1()
            self.band_blocks = []
            for i in range(self.band_number_of_blocks):
                self.band_blocks.append(Nitf.BandBlock(self._io, self, self._root))



    class ReservedExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved_extension_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.reserved_extension_date_and_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.reserved_extension_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.reserved_extension_security = Nitf.FileSecurity(self._io, self, self._root)
            self.reserved_extension_format = (self._io.read_bytes(3)).decode(u"ASCII")
            self.reserved_extension_extension = (self._io.read_bytes(3)).decode(u"ASCII")


    class DataExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data_extension_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.data_extension_date_and_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.data_extension_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.data_extension_security = Nitf.FileSecurity(self._io, self, self._root)
            self.data_extension_format = (self._io.read_bytes(3)).decode(u"ASCII")
            self.data_extension_extension = (self._io.read_bytes(3)).decode(u"ASCII")


    class ImageData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = (self._io.read_bytes_full()).decode(u"ASCII")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = Nitf.ImageData(self._io, self, self._root)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader = Nitf.TextSubheader(self._io, self, self._root)
            self.text_data = (self._io.read_bytes_full()).decode(u"ASCII")


    class BandBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_id = (self._io.read_bytes(2)).decode(u"ASCII")
            self.block_data = (self._io.read_bytes(8)).decode(u"ASCII")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data_extension_subheader = Nitf.DataExtensionSubheader(self._io, self, self._root)
            self.data_extension_data = (self._io.read_bytes_full()).decode(u"ASCII")


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_date_and_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.target_id = (self._io.read_bytes(17)).decode(u"ASCII")
            self.image_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.image_security = Nitf.FileSecurity(self._io, self, self._root)
            self.image_compression = (self._io.read_bytes(2)).decode(u"ASCII")
            self.image_color = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_representation = (self._io.read_bytes(8)).decode(u"ASCII")
            self.image_category = (self._io.read_bytes(8)).decode(u"ASCII")
            self.image_data_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.image_resolution_level = self._io.read_u1()
            self.image_location = (self._io.read_bytes(10)).decode(u"ASCII")
            self.image_magnification = (self._io.read_bytes(4)).decode(u"ASCII")
            self.image_coordinate_system = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_geographic_location = (self._io.read_bytes(60)).decode(u"ASCII")
            self.image_number_of_bands = self._io.read_u1()
            self.image_bands = []
            for i in range(self.image_number_of_bands):
                self.image_bands.append(Nitf.ImageBand(self._io, self, self._root))



    class FileSecurity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification_system = (self._io.read_bytes(2)).decode(u"ASCII")
            self.codewords = (self._io.read_bytes(11)).decode(u"ASCII")
            self.control_and_handling = (self._io.read_bytes(2)).decode(u"ASCII")
            self.releaseability = (self._io.read_bytes(20)).decode(u"ASCII")
            self.declass_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.declass_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.declass_exemption = (self._io.read_bytes(4)).decode(u"ASCII")
            self.downgrade = (self._io.read_bytes(1)).decode(u"ASCII")
            self.downgrade_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.classification_text = (self._io.read_bytes(43)).decode(u"ASCII")
            self.classification_authority_type = (self._io.read_bytes(1)).decode(u"ASCII")
            self.classification_authority = (self._io.read_bytes(40)).decode(u"ASCII")
            self.classification_reason = (self._io.read_bytes(1)).decode(u"ASCII")
            self.security_source_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.security_control_number = (self._io.read_bytes(15)).decode(u"ASCII")


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(2)).decode(u"ASCII")
            self.complex_level = self._io.read_u1()
            self.standard_type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.originating_station_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.file_date_and_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.file_security = Nitf.FileSecurity(self._io, self, self._root)
            self.file_copy_number = (self._io.read_bytes(5)).decode(u"ASCII")
            self.file_number_of_copies = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryption = self._io.read_u1()
            self.file_background_color = Nitf.RgbColor(self._io, self, self._root)
            self.originator_name = (self._io.read_bytes(24)).decode(u"ASCII")
            self.originator_phone = (self._io.read_bytes(18)).decode(u"ASCII")


    class RgbColor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u1()
            self.green = self._io.read_u1()
            self.blue = self._io.read_u1()



