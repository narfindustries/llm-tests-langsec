# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while True:
            _ = Jpeg.Segment(self._io, self, self._root)
            self.segments.append(_)
            if _.marker == 65497:
                break
            i += 1

    class Segment(KaitaiStruct):

        class Markers(Enum):
            sof0 = 192
            sof2 = 194
            dht = 196
            soi = 216
            eoi = 217
            sos = 218
            dqt = 219
            app0 = 224
            app1 = 225
            app2 = 226
            com = 254
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker_prefix = self._io.read_bytes(1)
            if not self.marker_prefix == b"\xFF":
                raise kaitaistruct.ValidationNotEqualError(b"\xFF", self.marker_prefix, self._io, u"/types/segment/seq/0")
            self.marker = self._io.read_u1()
            if self.marker != 217:
                self.length = self._io.read_u2be()

            if  ((self.marker != 217) and (self.marker != 216)) :
                self.data = self._io.read_bytes((self.length - 2))


        @property
        def marker_name(self):
            if hasattr(self, '_m_marker_name'):
                return self._m_marker_name

            self._m_marker_name = (u"SOI" if self.marker == 216 else (u"EOI" if self.marker == 217 else (u"SOS" if self.marker == 218 else (u"APP0" if self.marker == 224 else (u"APP1" if self.marker == 225 else (u"APP2" if self.marker == 226 else (u"DQT" if self.marker == 219 else (u"SOF0" if self.marker == 192 else (u"SOF2" if self.marker == 194 else (u"DHT" if self.marker == 196 else (u"COM" if self.marker == 254 else u"Unknown")))))))))))
            return getattr(self, '_m_marker_name', None)



