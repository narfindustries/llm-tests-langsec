# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Icmp(KaitaiStruct):

    class IcmpType(Enum):
        echo_reply = 0
        destination_unreachable = 3
        echo_request = 8
        time_exceeded = 11
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.type = KaitaiStream.resolve_enum(Icmp.IcmpType, self._io.read_u1())
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        self.rest_of_header = self._io.read_u4be()
        self.payload = (self._io.read_bytes_full()).decode(u"ascii")

    @property
    def header_length(self):
        """Fixed ICMP header length in bytes."""
        if hasattr(self, '_m_header_length'):
            return self._m_header_length

        self._m_header_length = 8
        return getattr(self, '_m_header_length', None)

    @property
    def is_valid_checksum(self):
        """Simple validation of checksum field."""
        if hasattr(self, '_m_is_valid_checksum'):
            return self._m_is_valid_checksum

        self._m_is_valid_checksum = self.checksum != 0
        return getattr(self, '_m_is_valid_checksum', None)


