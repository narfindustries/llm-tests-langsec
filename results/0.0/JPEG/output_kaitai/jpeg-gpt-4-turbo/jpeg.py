# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
    """JPEG File Interchange Format, or JFIF, is a graphical data format that enables the compression and storage of digital images using the JPEG standard.
    """

    class JpegMarker(Enum):
        sof0 = 192
        dht = 196
        soi = 216
        eoi = 217
        sos = 218
        dqt = 219
        dri = 221
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Jpeg.Segment(self._io, self, self._root))
            i += 1


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker_prefix = self._io.read_bytes(1)
            if not self.marker_prefix == b"\xFF":
                raise kaitaistruct.ValidationNotEqualError(b"\xFF", self.marker_prefix, self._io, u"/types/segment/seq/0")
            self.marker = KaitaiStream.resolve_enum(Jpeg.JpegMarker, self._io.read_u1())
            if  ((self.marker != Jpeg.JpegMarker.soi) and (self.marker != Jpeg.JpegMarker.eoi)) :
                self.length = self._io.read_u2le()

            if  ((self.marker != Jpeg.JpegMarker.soi) and (self.marker != Jpeg.JpegMarker.eoi)) :
                self.data = self._io.read_bytes((self.length - 2))




