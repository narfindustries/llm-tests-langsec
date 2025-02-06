# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsPacket(KaitaiStruct):
    """DNS (Domain Name System) is a hierarchical decentralized naming system for computers,
    services, or other resources connected to the Internet or a private network. It associates
    various information with domain names assigned to each of the participating entities.
    Most prominently, it translates more readily memorized domain names to the numerical
    IP addresses needed for locating and identifying computer services and devices with
    the underlying network protocols. By providing a worldwide, distributed directory service,
    the Domain Name System has been an essential component of the functionality of the Internet since 1985.
    This spec covers the structure of DNS packets, both query and response types.
    """

    class TypeEnum(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        ptr = 12
        mx = 15
        txt = 16
        aaaa = 28
        srv = 33
        any = 255

    class ClassEnum(Enum):
        in = 1
        ch = 3
        hs = 4
        any = 255
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
            self.answers.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.header.nscount):
            self.authorities.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.header.arcount):
            self.additionals.append(DnsPacket.ResourceRecord(self._io, self, self._root))


    class Header(KaitaiStruct):

        class Flags(Enum):
            rcode = 15
            z = 112
            ra = 128
            rd = 256
            tc = 512
            aa = 1024
            opcode = 30720
            qr = 32768
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transaction_id = self._io.read_u2be()
            self.flags = KaitaiStream.resolve_enum(DnsPacket.Header.Flags, self._io.read_u2be())
            self.qdcount = self._io.read_u2be()
            self.ancount = self._io.read_u2be()
            self.nscount = self._io.read_u2be()
            self.arcount = self._io.read_u2be()


    class Query(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeEnum, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.ClassEnum, self._io.read_u2be())


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeEnum, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.ClassEnum, self._io.read_u2be())
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
                _ = DnsPacket.DomainName.Label(self._io, self, self._root)
                self.parts.append(_)
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
                self.name = (self._io.read_bytes(self.length)).decode(u"ASCII")




