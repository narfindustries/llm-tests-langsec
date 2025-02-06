# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.type = self._io.read_u1()
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        self.rest_of_header = IcmpPacket.RestOfHeader(self._io, self, self._root)
        self.data = self._io.read_bytes_full()

    class RestOfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if  ((self._parent.type == 3) or (self._parent.type == 11) or (self._parent.type == 12)) :
                self.unused = self._io.read_u4be()

            if self._parent.type == 5:
                self.gateway_internet_address = IcmpPacket.Ipv4Address(self._io, self, self._root)

            if  ((self._parent.type == 0) or (self._parent.type == 8)) :
                self.echo = IcmpPacket.Echo(self._io, self, self._root)



    class Echo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()


    class Ipv4Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.b1 = self._io.read_u1()
            self.b2 = self._io.read_u1()
            self.b3 = self._io.read_u1()
            self.b4 = self._io.read_u1()

        @property
        def address(self):
            if hasattr(self, '_m_address'):
                return self._m_address

            self._m_address = str(self.b1) + u"." + str(self.b2) + u"." + str(self.b3) + u"." + str(self.b4)
            return getattr(self, '_m_address', None)



