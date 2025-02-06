# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NtpV4(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.li_vn_mode = self._io.read_u1()
        self.stratum = self._io.read_u1()
        self.poll = self._io.read_s1()
        self.precision = self._io.read_s1()
        self.root_delay = self._io.read_u4be()
        self.root_dispersion = self._io.read_u4be()
        self.reference_id = self._io.read_u4be()
        self.reference_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
        self.originate_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
        self.receive_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
        self.transmit_timestamp = NtpV4.NtpTimestamp(self._io, self, self._root)
        if (self._io.size() - self._io.pos()) >= 12:
            self.authenticator = NtpV4.Authenticator(self._io, self, self._root)


    class NtpTimestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seconds = self._io.read_u4be()
            self.fraction = self._io.read_u4be()


    class Authenticator(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key_identifier = self._io.read_u4be()
            self.message_digest = self._io.read_u8be()


    @property
    def li(self):
        if hasattr(self, '_m_li'):
            return self._m_li

        self._m_li = ((self.li_vn_mode >> 6) & 3)
        return getattr(self, '_m_li', None)

    @property
    def vn(self):
        if hasattr(self, '_m_vn'):
            return self._m_vn

        self._m_vn = ((self.li_vn_mode >> 3) & 7)
        return getattr(self, '_m_vn', None)

    @property
    def mode(self):
        if hasattr(self, '_m_mode'):
            return self._m_mode

        self._m_mode = (self.li_vn_mode & 7)
        return getattr(self, '_m_mode', None)


