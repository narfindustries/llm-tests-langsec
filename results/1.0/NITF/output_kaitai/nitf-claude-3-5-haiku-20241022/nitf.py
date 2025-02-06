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
            self.data_extension_segments.append(Nitf.DesSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.file_header.num_reserved_extension_segments):
            self.reserved_extension_segments.append(Nitf.ResSegment(self._io, self, self._root))


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tx = self._io.read_bytes(2)
            if not self.tx == b"\x54\x58":
                raise kaitaistruct.ValidationNotEqualError(b"\x54\x58", self.tx, self._io, u"/types/text_subheader/seq/0")
            self.text_security = Nitf.SecurityType(self._io, self, self._root)


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


    class ResSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_subheader = Nitf.ResSubheader(self._io, self, self._root)
            self.res_data = self._io.read_bytes_full()


    class DesSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.des_subheader = Nitf.DesSubheader(self._io, self, self._root)
            self.des_data = self._io.read_bytes_full()


    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gs = self._io.read_bytes(2)
            if not self.gs == b"\x47\x53":
                raise kaitaistruct.ValidationNotEqualError(b"\x47\x53", self.gs, self._io, u"/types/graphic_subheader/seq/0")
            self.graphic_security = Nitf.SecurityType(self._io, self, self._root)


    class SecurityType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.security_classification = (KaitaiStream.bytes_terminate(self._io.read_bytes(1), 0, False)).decode(u"ascii")
            self.security_country_code = (KaitaiStream.bytes_terminate(self._io.read_bytes(2), 0, False)).decode(u"ascii")
            self.security_release_marking = (KaitaiStream.bytes_terminate(self._io.read_bytes(6), 0, False)).decode(u"ascii")


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = self._io.read_bytes(2)
            if not self.im == b"\x49\x4D":
                raise kaitaistruct.ValidationNotEqualError(b"\x49\x4D", self.im, self._io, u"/types/image_subheader/seq/0")
            self.image_security = Nitf.SecurityType(self._io, self, self._root)
            self.encrypted = self._io.read_u1()
            self.image_source = (KaitaiStream.bytes_terminate(self._io.read_bytes(42), 0, False)).decode(u"ascii")
            self.number_of_significant_rows = self._io.read_u4be()
            self.number_of_significant_columns = self._io.read_u4be()
            self.pixel_type = (KaitaiStream.bytes_terminate(self._io.read_bytes(3), 0, False)).decode(u"ascii")
            self.image_representation = (KaitaiStream.bytes_terminate(self._io.read_bytes(8), 0, False)).decode(u"ascii")
            self.image_category = (KaitaiStream.bytes_terminate(self._io.read_bytes(3), 0, False)).decode(u"ascii")


    class ResSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.re = self._io.read_bytes(2)
            if not self.re == b"\x52\x45":
                raise kaitaistruct.ValidationNotEqualError(b"\x52\x45", self.re, self._io, u"/types/res_subheader/seq/0")
            self.res_security = Nitf.SecurityType(self._io, self, self._root)


    class DesSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.de = self._io.read_bytes(2)
            if not self.de == b"\x44\x45":
                raise kaitaistruct.ValidationNotEqualError(b"\x44\x45", self.de, self._io, u"/types/des_subheader/seq/0")
            self.des_security = Nitf.SecurityType(self._io, self, self._root)


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
            self.fhdr = self._io.read_bytes(4)
            if not self.fhdr == b"\x4E\x49\x54\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x4E\x49\x54\x46", self.fhdr, self._io, u"/types/file_header/seq/0")
            self.fver = (KaitaiStream.bytes_terminate(self._io.read_bytes(5), 0, False)).decode(u"ascii")
            self.clevel = self._io.read_u1()
            self.stype = Nitf.SecurityType(self._io, self, self._root)
            self.orig = (KaitaiStream.bytes_terminate(self._io.read_bytes(10), 0, False)).decode(u"ascii")
            self.fdt = (KaitaiStream.bytes_terminate(self._io.read_bytes(14), 0, False)).decode(u"ascii")
            self.ftitle = (KaitaiStream.bytes_terminate(self._io.read_bytes(80), 0, False)).decode(u"ascii")
            self.fscop = self._io.read_u2be()
            self.fscpys = self._io.read_u2be()
            self.encryption = (KaitaiStream.bytes_terminate(self._io.read_bytes(1), 0, False)).decode(u"ascii")
            self.num_image_segments = self._io.read_u4be()
            self.num_graphic_segments = self._io.read_u4be()
            self.num_text_segments = self._io.read_u4be()
            self.num_data_extension_segments = self._io.read_u4be()
            self.num_reserved_extension_segments = self._io.read_u4be()
            self.user_header_length = self._io.read_u4be()
            self.extended_header_length = self._io.read_u4be()



