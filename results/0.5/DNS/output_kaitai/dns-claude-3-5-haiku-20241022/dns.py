# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Dns(KaitaiStruct):

    class QueryType(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        ptr = 12
        mx = 15
        aaaa = 28
        axfr = 252
        any = 255

    class QueryClass(Enum):
        in = 1
        ch = 3
        hs = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2be()
        self.flags = Dns.FlagsStruct(self._io, self, self._root)
        self.questions_count = self._io.read_u2be()
        self.answer_count = self._io.read_u2be()
        self.authority_count = self._io.read_u2be()
        self.additional_count = self._io.read_u2be()
        self.queries = []
        for i in range(self.questions_count):
            self.queries.append(Dns.Query(self._io, self, self._root))

        self.answers = []
        for i in range(self.answer_count):
            self.answers.append(Dns.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.authority_count):
            self.authorities.append(Dns.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.additional_count):
            self.additionals.append(Dns.ResourceRecord(self._io, self, self._root))


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(Dns.QueryType, self._io.read_u2be())
            self.record_class = KaitaiStream.resolve_enum(Dns.QueryClass, self._io.read_u2be())
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == Dns.QueryType.aaaa:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.Ipv6Address(_io__raw_rdata, self, self._root)
            elif _on == Dns.QueryType.a:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.Ipv4Address(_io__raw_rdata, self, self._root)
            elif _on == Dns.QueryType.mx:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.MxRecord(_io__raw_rdata, self, self._root)
            elif _on == Dns.QueryType.cname:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.DomainName(_io__raw_rdata, self, self._root)
            else:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.RawData(_io__raw_rdata, self, self._root)


    class NamePart(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            if self.length > 0:
                self.part = (self._io.read_bytes(self.length)).decode(u"ascii")



    class FlagsStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qr = self._io.read_bits_int_be(1) != 0
            self.opcode = self._io.read_bits_int_be(4)
            self.aa = self._io.read_bits_int_be(1) != 0
            self.tc = self._io.read_bits_int_be(1) != 0
            self.rd = self._io.read_bits_int_be(1) != 0
            self.ra = self._io.read_bits_int_be(1) != 0
            self.z = self._io.read_bits_int_be(3)
            self.rcode = self._io.read_bits_int_be(4)


    class Ipv6Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(8):
                self.address.append(self._io.read_u2be())



    class RawData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = []
            i = 0
            while not self._io.is_eof():
                self.data.append(self._io.read_u1())
                i += 1



    class Query(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(Dns.QueryType, self._io.read_u2be())
            self.query_class = KaitaiStream.resolve_enum(Dns.QueryClass, self._io.read_u2be())


    class DomainName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_parts = []
            i = 0
            while True:
                _ = Dns.NamePart(self._io, self, self._root)
                self.name_parts.append(_)
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
            self.exchange = Dns.DomainName(self._io, self, self._root)


    class Ipv4Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(4):
                self.address.append(self._io.read_u1())




