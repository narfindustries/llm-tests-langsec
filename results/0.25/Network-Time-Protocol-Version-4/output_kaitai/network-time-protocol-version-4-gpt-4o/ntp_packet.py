# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpPacket(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.li_vn_mode = self._io.read_u1()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_u1()
        self.precision = self._io.read_u1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_identifier = self._io.read_u4be()
        self.reference_timestamp = self._io.read_u8be()
        self.originate_timestamp = self._io.read_u8be()
        self.receive_timestamp = self._io.read_u8be()
        self.transmit_timestamp = self._io.read_u8be()

    @property
    def leap_indicator(self):
        if hasattr(self, '_m_leap_indicator'):
            return self._m_leap_indicator

        self._m_leap_indicator = ((self.li_vn_mode >> 6) & 3)
        return getattr(self, '_m_leap_indicator', None)

    @property
    def version_number(self):
        if hasattr(self, '_m_version_number'):
            return self._m_version_number

        self._m_version_number = ((self.li_vn_mode >> 3) & 7)
        return getattr(self, '_m_version_number', None)

    @property
    def mode(self):
        if hasattr(self, '_m_mode'):
            return self._m_mode

        self._m_mode = (self.li_vn_mode & 7)
        return getattr(self, '_m_mode', None)


