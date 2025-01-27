# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):

    class MarkerTypes(Enum):
        start_of_frame_baseline = 192
        start_of_frame_progressive = 194
        define_huffman_table = 196
        start_of_image = 216
        end_of_image = 217
        start_of_scan = 218
        define_quantization_table = 219
        define_restart_interval = 221
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while True:
            _ = Jpeg.JpegSegment(self._io, self, self._root)
            self.segments.append(_)
            if _.marker != 255:
                break
            i += 1

    class JpegSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u1()
            if  ((self.marker != 216) and (self.marker != 217)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != 216) and (self.marker != 217) and (self.marker != 1)) :
                self.data = self._io.read_bytes((self.length - 2))




