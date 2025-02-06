# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsMessage(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2be()
        self.flags = DnsMessage.DnsFlags(self._io, self, self._root)
        self.qdcount = self._io.read_u2be()
        self.ancount = self._io.read_u2be()
        self.nscount = self._io.read_u2be()
        self.arcount = self._io.read_u2be()
        self.questions = []
        for i in range(self.qdcount):
            self.questions.append(DnsMessage.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.ancount):
            self.answers.append(DnsMessage.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.nscount):
            self.authorities.append(DnsMessage.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.arcount):
            self.additionals.append(DnsMessage.ResourceRecord(self._io, self, self._root))


    class DnsFlags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flags_raw = self._io.read_u2be()

        @property
        def qr(self):
            if hasattr(self, '_m_qr'):
                return self._m_qr

            self._m_qr = ((self.flags_raw >> 15) & 1)
            return getattr(self, '_m_qr', None)

        @property
        def ra(self):
            if hasattr(self, '_m_ra'):
                return self._m_ra

            self._m_ra = ((self.flags_raw >> 7) & 1)
            return getattr(self, '_m_ra', None)

        @property
        def tc(self):
            if hasattr(self, '_m_tc'):
                return self._m_tc

            self._m_tc = ((self.flags_raw >> 9) & 1)
            return getattr(self, '_m_tc', None)

        @property
        def rcode(self):
            if hasattr(self, '_m_rcode'):
                return self._m_rcode

            self._m_rcode = (self.flags_raw & 15)
            return getattr(self, '_m_rcode', None)

        @property
        def opcode(self):
            if hasattr(self, '_m_opcode'):
                return self._m_opcode

            self._m_opcode = ((self.flags_raw >> 11) & 15)
            return getattr(self, '_m_opcode', None)

        @property
        def aa(self):
            if hasattr(self, '_m_aa'):
                return self._m_aa

            self._m_aa = ((self.flags_raw >> 10) & 1)
            return getattr(self, '_m_aa', None)

        @property
        def rd(self):
            if hasattr(self, '_m_rd'):
                return self._m_rd

            self._m_rd = ((self.flags_raw >> 8) & 1)
            return getattr(self, '_m_rd', None)

        @property
        def z_reserved(self):
            if hasattr(self, '_m_z_reserved'):
                return self._m_z_reserved

            self._m_z_reserved = ((self.flags_raw >> 4) & 7)
            return getattr(self, '_m_z_reserved', None)


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qname = DnsMessage.DomainName(self._io, self, self._root)
            self.qtype = self._io.read_u2be()
            self.qclass = self._io.read_u2be()


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsMessage.DomainName(self._io, self, self._root)
            self.type = self._io.read_u2be()
            self.class_ = self._io.read_u2be()
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
            self.parts = []
            i = 0
            while True:
                _ = DnsMessage.DomainName.DomainLabel(self._io, self, self._root)
                self.parts.append(_)
                if _.is_root:
                    break
                i += 1

        class DomainLabel(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.length = self._io.read_u1()
                self.name = self._io.read_bytes(self.length)

            @property
            def is_root(self):
                if hasattr(self, '_m_is_root'):
                    return self._m_is_root

                self._m_is_root = self.length == 0
                return getattr(self, '_m_is_root', None)




