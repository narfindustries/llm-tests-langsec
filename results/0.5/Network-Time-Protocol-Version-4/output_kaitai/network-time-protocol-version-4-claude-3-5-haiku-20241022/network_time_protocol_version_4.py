# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NetworkTimeProtocolVersion4(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.leap_indicator = self._io.read_bits_int_be(2)
        self.version = self._io.read_bits_int_be(3)
        self.mode = self._io.read_bits_int_be(3)
        self._io.align_to_byte()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_f4be()
        self.root_dispersion = self._io.read_f4be()
        self.reference_identifier = (self._io.read_bytes(4)).decode(u"ASCII")
        self.reference_timestamp = self._io.read_u8be()
        self.originate_timestamp = self._io.read_u8be()
        self.receive_timestamp = self._io.read_u8be()
        self.transmit_timestamp = self._io.read_u8be()

    @property
    def ntp_timestamp(self):
        """Convenience accessor for transmit timestamp."""
        if hasattr(self, '_m_ntp_timestamp'):
            return self._m_ntp_timestamp

        self._m_ntp_timestamp = self.transmit_timestamp
        return getattr(self, '_m_ntp_timestamp', None)


