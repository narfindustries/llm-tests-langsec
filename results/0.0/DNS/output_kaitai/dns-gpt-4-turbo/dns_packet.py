# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsPacket(KaitaiStruct):

    class FlagsType(Enum):
        cd = 1
        qr = 256
        opcode = 512
        aa = 1024
        tc = 2048
        rd = 4096
        ra = 8192
        z = 16384
        ad = 32768

    class TypeEnum(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        mx = 15
        txt = 16
        aaaa = 28

    class ClassEnum(Enum):
        in = 1
        cs = 2
        ch = 3
        hs = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2be()
        self.flags = KaitaiStream.resolve_enum(DnsPacket.FlagsType, self._io.read_u2be())
        self.qdcount = self._io.read_u2be()
        self.ancount = self._io.read_u2be()
        self.nscount = self._io.read_u2be()
        self.arcount = self._io.read_u2be()
        self.queries = []
        for i in range(self.qdcount):
            self.queries.append(DnsPacket.Query(self._io, self, self._root))

        self.answers = []
        for i in range(self.ancount):
            self.answers.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.nscount):
            self.authorities.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.arcount):
            self.additionals.append(DnsPacket.ResourceRecord(self._io, self, self._root))


    class TxtRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text = (self._io.read_bytes_full()).decode(u"ASCII")


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeEnum, self._io.read_u2be())
            self.rr_class = KaitaiStream.resolve_enum(DnsPacket.ClassEnum, self._io.read_u2be())
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == DnsPacket.TypeEnum.txt:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.TxtRecord(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeEnum.a:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.AddressRecord(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeEnum.cname:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.CnameRecord(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeEnum.mx:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.MxRecord(_io__raw_rdata, self, self._root)
            else:
                self.rdata = self._io.read_bytes(self.rdlength)


    class AddressRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = self._io.read_u4be()


    class CnameRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.cname = DnsPacket.DomainName(self._io, self, self._root)


    class Label(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.name = (self._io.read_bytes(self.length)).decode(u"ASCII")


    class Query(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeEnum, self._io.read_u2be())
            self.query_class = KaitaiStream.resolve_enum(DnsPacket.ClassEnum, self._io.read_u2be())


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
                _ = DnsPacket.Label(self._io, self, self._root)
                self.parts.append(_)
                if _.length == 0:
                    break
                i += 1


    class MxRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.preference = self._io.read_u2be()
            self.exchange = DnsPacket.DomainName(self._io, self, self._root)



