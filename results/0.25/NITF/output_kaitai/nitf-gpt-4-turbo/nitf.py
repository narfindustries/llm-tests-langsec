# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """NITF is a file format created by the US Government for transmission of images and associated metadata.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.header.num_linfo):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphics_segments = []
        for i in range(self.header.num_graphics_segments):
            self.graphics_segments.append(Nitf.GraphicsSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.header.num_text_files):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.header.num_data_extension):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.header.num_reserved_extension):
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))


    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.ReservedSubHeader(self._io, self, self._root)
            self.data = self._io.read_bytes(self.header.data_length)


    class LengthInfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length_image_segment = (self._io.read_bytes(6)).decode(u"ASCII")
            self.image_header_length = (self._io.read_bytes(4)).decode(u"ASCII")


    class ImageBand(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.representation = (self._io.read_bytes(2)).decode(u"ASCII")
            self.subrange = (self._io.read_bytes(6)).decode(u"ASCII")
            self.num_luts = self._io.read_u1()
            self.lut_value_length = (self._io.read_bytes(5)).decode(u"ASCII")
            self.luts = []
            for i in range(self.num_luts):
                self.luts.append(Nitf.Lut(self._io, self, self._root))



    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.ImageSubHeader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.header.image_data_length)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.TextSubHeader(self._io, self, self._root)
            self.text_data = self._io.read_bytes(self.header.text_data_length)


    class GraphicsSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.GraphicsSubHeader(self._io, self, self._root)
            self.graphics_data = self._io.read_bytes(self.header.graphics_data_length)


    class ClasSecurity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.clas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.security_system = (self._io.read_bytes(2)).decode(u"ASCII")
            self.codewords = (self._io.read_bytes(11)).decode(u"ASCII")
            self.control_and_handling = (self._io.read_bytes(2)).decode(u"ASCII")
            self.releasability = (self._io.read_bytes(20)).decode(u"ASCII")
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


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.DataExtensionSubHeader(self._io, self, self._root)
            self.data = self._io.read_bytes(self.header.data_length)


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
            self.image_security = Nitf.ClasSecurity(self._io, self, self._root)
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_source = (self._io.read_bytes(42)).decode(u"ASCII")
            self.num_sig_rows = self._io.read_u4be()
            self.num_sig_cols = self._io.read_u4be()
            self.pixel_value_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.image_representation = (self._io.read_bytes(8)).decode(u"ASCII")
            self.image_category = (self._io.read_bytes(8)).decode(u"ASCII")
            self.actual_bits_per_pixel = (self._io.read_bytes(2)).decode(u"ASCII")
            self.pixel_justification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_coordinate_rep = (self._io.read_bytes(1)).decode(u"ASCII")
            self.image_geo_loc = (self._io.read_bytes(60)).decode(u"ASCII")
            self.num_image_bands = self._io.read_u1()
            self.image_bands = []
            for i in range(self.num_image_bands):
                self.image_bands.append(Nitf.ImageBand(self._io, self, self._root))

            self.image_data_length = self._io.read_u4be()


    class ReservedSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_type_id = (self._io.read_bytes(25)).decode(u"ASCII")
            self.res_version = (self._io.read_bytes(2)).decode(u"ASCII")
            self.res_class = Nitf.ClasSecurity(self._io, self, self._root)
            self.data_length = self._io.read_u4be()


    class TextSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_id = (self._io.read_bytes(7)).decode(u"ASCII")
            self.text_date_time = (self._io.read_bytes(14)).decode(u"ASCII")
            self.text_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.text_security = Nitf.ClasSecurity(self._io, self, self._root)
            self.text_format = (self._io.read_bytes(3)).decode(u"ASCII")
            self.text_data_length = self._io.read_u4be()


    class GraphicsSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.graphic_name = (self._io.read_bytes(20)).decode(u"ASCII")
            self.graphic_classification = Nitf.ClasSecurity(self._io, self, self._root)
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.graphic_type = (self._io.read_bytes(1)).decode(u"ASCII")
            self.graphic_display_level = (self._io.read_bytes(3)).decode(u"ASCII")
            self.graphic_attachment_level = (self._io.read_bytes(3)).decode(u"ASCII")
            self.graphic_date = (self._io.read_bytes(14)).decode(u"ASCII")
            self.graphic_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.graphic_security = Nitf.ClasSecurity(self._io, self, self._root)
            self.graphics_data_length = self._io.read_u4be()


    class DataExtensionSubHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.desid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.data_item_overflow = (self._io.read_bytes(3)).decode(u"ASCII")
            self.des_version = (self._io.read_bytes(2)).decode(u"ASCII")
            self.declas = Nitf.ClasSecurity(self._io, self, self._root)
            self.data_length = self._io.read_u4be()


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
            self.file_security = Nitf.ClasSecurity(self._io, self, self._root)
            self.file_copy_number = (self._io.read_bytes(5)).decode(u"ASCII")
            self.file_num_of_copys = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryption = (self._io.read_bytes(1)).decode(u"ASCII")
            self.file_bg_color = (self._io.read_bytes(3)).decode(u"ASCII")
            self.originator_name = (self._io.read_bytes(24)).decode(u"ASCII")
            self.originator_phone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.file_length = (self._io.read_bytes(12)).decode(u"ASCII")
            self.header_length = (self._io.read_bytes(6)).decode(u"ASCII")
            self.num_linfo = self._io.read_u2be()
            self.linfo = []
            for i in range(self.num_linfo):
                self.linfo.append(Nitf.LengthInfo(self._io, self, self._root))

            self.num_graphics_segments = self._io.read_u2be()
            self.num_text_files = self._io.read_u2be()
            self.num_data_extension = self._io.read_u2be()
            self.num_reserved_extension = self._io.read_u2be()


    class Lut(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.num_lut_data = self._io.read_u4be()
            self.lut_data = []
            for i in range(self.num_lut_data):
                self.lut_data.append(self._io.read_u1())




