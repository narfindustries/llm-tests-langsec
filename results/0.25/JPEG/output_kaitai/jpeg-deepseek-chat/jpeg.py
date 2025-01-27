# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.soi = self._io.read_u2be()
        if not self.soi == 65496:
            raise kaitaistruct.ValidationNotEqualError(65496, self.soi, self._io, u"/seq/0")
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
            self.marker = self._io.read_u2be()
            if  ((self.marker != 65498) and (self.marker != 65497)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != 65498) and (self.marker != 65497)) :
                self.data = self._io.read_bytes((self.length - 2))

            if self.marker == 65498:
                self.scan_data = self._io.read_bytes_full()


        @property
        def marker_name(self):
            if hasattr(self, '_m_marker_name'):
                return self._m_marker_name

            self._m_marker_name = (u"SOF0" if self.marker == 65472 else (u"SOF1" if self.marker == 65473 else (u"SOF2" if self.marker == 65474 else (u"SOF3" if self.marker == 65475 else (u"DHT" if self.marker == 65476 else (u"SOF5" if self.marker == 65477 else (u"SOF6" if self.marker == 65478 else (u"SOF7" if self.marker == 65479 else (u"JPG" if self.marker == 65480 else (u"SOF9" if self.marker == 65481 else (u"SOF10" if self.marker == 65482 else (u"SOF11" if self.marker == 65483 else (u"DAC" if self.marker == 65484 else (u"SOF13" if self.marker == 65485 else (u"SOF14" if self.marker == 65486 else (u"SOF15" if self.marker == 65487 else (u"RST0" if self.marker == 65488 else (u"RST1" if self.marker == 65489 else (u"RST2" if self.marker == 65490 else (u"RST3" if self.marker == 65491 else (u"RST4" if self.marker == 65492 else (u"RST5" if self.marker == 65493 else (u"RST6" if self.marker == 65494 else (u"RST7" if self.marker == 65495 else (u"SOI" if self.marker == 65496 else (u"EOI" if self.marker == 65497 else (u"SOS" if self.marker == 65498 else (u"DQT" if self.marker == 65499 else (u"DNL" if self.marker == 65500 else (u"DRI" if self.marker == 65501 else (u"DHP" if self.marker == 65502 else (u"EXP" if self.marker == 65503 else (u"APP0" if self.marker == 65504 else (u"APP1" if self.marker == 65505 else (u"APP2" if self.marker == 65506 else (u"APP3" if self.marker == 65507 else (u"APP4" if self.marker == 65508 else (u"APP5" if self.marker == 65509 else (u"APP6" if self.marker == 65510 else (u"APP7" if self.marker == 65511 else (u"APP8" if self.marker == 65512 else (u"APP9" if self.marker == 65513 else (u"APP10" if self.marker == 65514 else (u"APP11" if self.marker == 65515 else (u"APP12" if self.marker == 65516 else (u"APP13" if self.marker == 65517 else (u"APP14" if self.marker == 65518 else (u"APP15" if self.marker == 65519 else (u"JPG0" if self.marker == 65520 else (u"JPG1" if self.marker == 65521 else (u"JPG2" if self.marker == 65522 else (u"JPG3" if self.marker == 65523 else (u"JPG4" if self.marker == 65524 else (u"JPG5" if self.marker == 65525 else (u"JPG6" if self.marker == 65526 else (u"JPG7" if self.marker == 65527 else (u"JPG8" if self.marker == 65528 else (u"JPG9" if self.marker == 65529 else (u"JPG10" if self.marker == 65530 else (u"JPG11" if self.marker == 65531 else (u"JPG12" if self.marker == 65532 else (u"JPG13" if self.marker == 65533 else (u"COM" if self.marker == 65534 else u"Unknown")))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
            return getattr(self, '_m_marker_name', None)



