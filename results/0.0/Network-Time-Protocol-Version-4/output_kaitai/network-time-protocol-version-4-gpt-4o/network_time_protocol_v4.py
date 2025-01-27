# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NetworkTimeProtocolV4(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.flags = NetworkTimeProtocolV4.Flags(self._io, self, self._root)
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = NetworkTimeProtocolV4.FixedPoint1616(self._io, self, self._root)
        self.root_dispersion = NetworkTimeProtocolV4.FixedPoint1616(self._io, self, self._root)
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = NetworkTimeProtocolV4.Timestamp(self._io, self, self._root)
        self.originate_timestamp = NetworkTimeProtocolV4.Timestamp(self._io, self, self._root)
        self.receive_timestamp = NetworkTimeProtocolV4.Timestamp(self._io, self, self._root)
        self.transmit_timestamp = NetworkTimeProtocolV4.Timestamp(self._io, self, self._root)

    class Flags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.leap_indicator = self._io.read_bits_int_be(2)
            self.version_number = self._io.read_bits_int_be(3)
            self.mode = self._io.read_bits_int_be(3)


    class FixedPoint1616(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.int_part = self._io.read_s2be()
            self.frac_part = self._io.read_u2be()


    class Timestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()



