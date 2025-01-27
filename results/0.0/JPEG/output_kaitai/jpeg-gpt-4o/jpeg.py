# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Jpeg(KaitaiStruct):

    class Marker(Enum):
        tem = 65281
        sof0 = 65472
        sof1 = 65473
        sof2 = 65474
        sof3 = 65475
        dht = 65476
        sof5 = 65477
        sof6 = 65478
        sof7 = 65479
        sof9 = 65481
        sof10 = 65482
        sof11 = 65483
        sof13 = 65485
        sof14 = 65486
        sof15 = 65487
        soi = 65496
        eoi = 65497
        sos = 65498
        dqt = 65499
        dri = 65501
        app0 = 65504
        app1 = 65505
        app2 = 65506
        app3 = 65507
        app4 = 65508
        app5 = 65509
        app6 = 65510
        app7 = 65511
        app8 = 65512
        app9 = 65513
        app10 = 65514
        app11 = 65515
        app12 = 65516
        app13 = 65517
        app14 = 65518
        app15 = 65519
        rst0 = 65520
        rst1 = 65521
        rst2 = 65522
        rst3 = 65523
        rst4 = 65524
        rst5 = 65525
        rst6 = 65526
        rst7 = 65527
        rst8 = 65528
        rst9 = 65529
        rst10 = 65530
        rst11 = 65531
        rst12 = 65532
        rst13 = 65533
        com = 65534
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
            if _.marker == 217:
                break
            i += 1

    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u2be()
            if  ((self.marker != 216) and (self.marker != 217)) :
                self.length = self._io.read_u2be()

            if  ((self.marker != 216) and (self.marker != 217)) :
                self.data = self._io.read_bytes((self.length - 2))




