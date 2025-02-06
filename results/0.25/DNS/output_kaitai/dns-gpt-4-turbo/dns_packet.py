# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsPacket(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = DnsPacket.Header(self._io, self, self._root)
        self.queries = []
        for i in range(self.header.qdcount):
            self.queries.append(DnsPacket.Query(self._io, self, self._root))

        self.answers = []
        for i in range(self.header.ancount):
            self.answers.append(DnsPacket.Rr(self._io, self, self._root))

        self.authorities = []
        for i in range(self.header.nscount):
            self.authorities.append(DnsPacket.Rr(self._io, self, self._root))

        self.additionals = []
        for i in range(self.header.arcount):
            self.additionals.append(DnsPacket.Rr(self._io, self, self._root))


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transaction_id = self._io.read_u2be()
            self.flags = self._io.read_u2be()
            self.qdcount = self._io.read_u2be()
            self.ancount = self._io.read_u2be()
            self.nscount = self._io.read_u2be()
            self.arcount = self._io.read_u2be()

        @property
        def qr(self):
            if hasattr(self, '_m_qr'):
                return self._m_qr

            self._m_qr = (self.flags >> 15)
            return getattr(self, '_m_qr', None)

        @property
        def ra(self):
            if hasattr(self, '_m_ra'):
                return self._m_ra

            self._m_ra = ((self.flags >> 7) & 1)
            return getattr(self, '_m_ra', None)

        @property
        def tc(self):
            if hasattr(self, '_m_tc'):
                return self._m_tc

            self._m_tc = ((self.flags >> 9) & 1)
            return getattr(self, '_m_tc', None)

        @property
        def rcode(self):
            if hasattr(self, '_m_rcode'):
                return self._m_rcode

            self._m_rcode = (self.flags & 15)
            return getattr(self, '_m_rcode', None)

        @property
        def opcode(self):
            if hasattr(self, '_m_opcode'):
                return self._m_opcode

            self._m_opcode = ((self.flags >> 11) & 15)
            return getattr(self, '_m_opcode', None)

        @property
        def aa(self):
            if hasattr(self, '_m_aa'):
                return self._m_aa

            self._m_aa = ((self.flags >> 10) & 1)
            return getattr(self, '_m_aa', None)

        @property
        def z(self):
            if hasattr(self, '_m_z'):
                return self._m_z

            self._m_z = ((self.flags >> 4) & 7)
            return getattr(self, '_m_z', None)

        @property
        def rd(self):
            if hasattr(self, '_m_rd'):
                return self._m_rd

            self._m_rd = ((self.flags >> 8) & 1)
            return getattr(self, '_m_rd', None)


    class Query(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qname = DnsPacket.DomainName(self._io, self, self._root)
            self.qtype = self._io.read_u2be()
            self.qclass = self._io.read_u2be()


    class Rr(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = self._io.read_u2be()
            self.class = self._io.read_u2be()
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            self.rdata = self._io.read_bytes(self.rdlength)


    class DomainName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = []
            i = 0
            while True:
                _ = DnsPacket.DomainName.Label(self._io, self, self._root)
                self.name.append(_)
                if _.length == 0:
                    break
                i += 1

        class Label(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.length = self._io.read_u1()
                if self.length > 0:
                    self.name = (self._io.read_bytes(self.length)).decode(u"ASCII")





