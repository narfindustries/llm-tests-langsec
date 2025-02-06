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
        self.questions = []
        for i in range(self.header.qdcount):
            self.questions.append(DnsPacket.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.header.ancount):
            self.answers.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.header.nscount):
            self.authorities.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.header.arcount):
            self.additionals.append(DnsPacket.ResourceRecord(self._io, self, self._root))


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u2be()
            self.flags = self._io.read_u2be()
            self.qdcount = self._io.read_u2be()
            self.ancount = self._io.read_u2be()
            self.nscount = self._io.read_u2be()
            self.arcount = self._io.read_u2be()


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qname = DnsPacket.DomainName(self._io, self, self._root)
            self.qtype = self._io.read_u2be()
            self.qclass = self._io.read_u2be()


    class ResourceRecord(KaitaiStruct):
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
            while not self._io.is_eof():
                self.name.append((self._io.read_bytes_term(0, False, True, True)).decode(u"ascii"))
                i += 1




