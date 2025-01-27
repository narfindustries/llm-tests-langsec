# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """National Imagery Transmission Format (NITF) specification for geospatial imagery files
    """
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


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_id = self._io.read_bytes(4)
            if not self.header_id == b"\x4E\x49\x54\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x4E\x49\x54\x46", self.header_id, self._io, u"/types/file_header/seq/0")
            self.header_version = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.file_date_time = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.file_title = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.file_length = self._io.read_u4be()
            self.header_length = self._io.read_u4be()
            self.num_image_segments = self._io.read_u2be()
            self.num_graphic_segments = self._io.read_u2be()
            self.num_text_segments = self._io.read_u2be()
            self.num_data_extension_segments = self._io.read_u2be()
            self.num_reserved_extension_segments = self._io.read_u2be()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.ImageHeader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.header.image_length)


    class ImageHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header_id = self._io.read_bytes(2)
            if not self.header_id == b"\x44\x45":
                raise kaitaistruct.ValidationNotEqualError(b"\x44\x45", self.header_id, self._io, u"/types/image_header/seq/0")
            self.image_title = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.image_length = self._io.read_u4be()
            self.image_width = self._io.read_u4be()
            self.image_height = self._io.read_u4be()
            self.pixel_type = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.pixel_bits = self._io.read_u2be()



