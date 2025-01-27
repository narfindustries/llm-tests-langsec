# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
    """JPEG is a commonly used method of lossy compression for digital images, particularly for those images produced by digital photography. JPEG's compression algorithm is at its best on photographs and paintings of realistic scenes with smooth variations of tone and color.
    """

    class MarkerEnum(Enum):
        tem = 1
        sof0 = 192
        sof2 = 194
        sof3 = 195
        dht = 196
        sof5 = 197
        sof6 = 198
        sof7 = 199
        jpg = 200
        sof9 = 201
        sof10 = 202
        sof11 = 203
        sof13 = 205
        sof14 = 206
        sof15 = 207
        soi = 216
        eoi = 217
        sos = 218
        dqt = 219
        dri = 221
        app0 = 224
        app1 = 225
        app2 = 226
        app3 = 227
        app4 = 228
        app5 = 229
        app6 = 230
        app7 = 231
        app8 = 232
        app9 = 233
        appa = 234
        appb = 235
        appc = 236
        appd = 237
        appe = 238
        appf = 239
        jpg0 = 240
        jpg1 = 241
        jpg2 = 242
        jpg3 = 243
        jpg4 = 244
        jpg5 = 245
        jpg6 = 246
        jpg7 = 247
        jpg8 = 248
        jpg9 = 249
        jpga = 250
        jpgb = 251
        jpgc = 252
        jpgd = 253
        com = 254
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
            self.magic = self._io.read_bytes(1)
            if not self.magic == b"\xFF":
                raise kaitaistruct.ValidationNotEqualError(b"\xFF", self.magic, self._io, u"/types/segment/seq/0")
            self.marker = KaitaiStream.resolve_enum(Jpeg.MarkerEnum, self._io.read_u1())
            if  ((self.marker != Jpeg.MarkerEnum.soi) and (self.marker != Jpeg.MarkerEnum.eoi)) :
                self.length = self._io.read_u2be()

            if self.length > 2:
                self.data = self._io.read_bytes((self.length - 2))




