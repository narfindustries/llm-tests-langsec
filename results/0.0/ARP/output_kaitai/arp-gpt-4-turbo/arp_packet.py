# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ArpPacket(KaitaiStruct):
    """The Address Resolution Protocol (ARP) is a network layer protocol
    used for mapping an IP address to a physical machine address that is
    recognized in the local network.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.htype = self._io.read_u2be()
        self.ptype = self._io.read_u2be()
        self.hlen = self._io.read_u1()
        self.plen = self._io.read_u1()
        self.oper = self._io.read_u2be()
        self.sha = self._io.read_bytes(self.sha_size)
        self.spa = self._io.read_bytes(self.spa_size)
        self.tha = self._io.read_bytes(self.tha_size)
        self.tpa = self._io.read_bytes(self.tpa_size)

    @property
    def sha_size(self):
        if hasattr(self, '_m_sha_size'):
            return self._m_sha_size

        self._m_sha_size = self.hlen
        return getattr(self, '_m_sha_size', None)

    @property
    def spa_size(self):
        if hasattr(self, '_m_spa_size'):
            return self._m_spa_size

        self._m_spa_size = self.plen
        return getattr(self, '_m_spa_size', None)

    @property
    def tha_size(self):
        if hasattr(self, '_m_tha_size'):
            return self._m_tha_size

        self._m_tha_size = self.hlen
        return getattr(self, '_m_tha_size', None)

    @property
    def tpa_size(self):
        if hasattr(self, '_m_tpa_size'):
            return self._m_tpa_size

        self._m_tpa_size = self.plen
        return getattr(self, '_m_tpa_size', None)


