# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):

    class Markers(Enum):
        sof0 = 65472
        dht = 65476
        soi = 65496
        eoi = 65497
        sos = 65498
        dqt = 65499
        app0 = 65504
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
            self.marker = KaitaiStream.resolve_enum(Jpeg.Markers, self._io.read_u2le())
            if  ((self.marker != Jpeg.Markers.soi) and (self.marker != Jpeg.Markers.eoi)) :
                self.length = self._io.read_u2le()

            if self.length > 2:
                self.data = self._io.read_bytes((self.length - 2))




