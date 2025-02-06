# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsPacket(KaitaiStruct):

    class TypeType(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        ptr = 12
        hinfo = 13
        mx = 15
        aaaa = 28
        axfr = 252
        any = 255

    class ClassType(Enum):
        in = 1
        cs = 2
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


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeType, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.ClassType, self._io.read_u2be())


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeType, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.ClassType, self._io.read_u2be())
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == DnsPacket.TypeType.a:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataA(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.cname:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.ns:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.soa:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataSoa(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.mx:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataMx(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.ptr:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.TypeType.aaaa:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataAaaa(_io__raw_rdata, self, self._root)
            else:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataUnknown(_io__raw_rdata, self, self._root)


    class RdataAaaa(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ip_addr = self._io.read_bytes(16)


    class Flags(KaitaiStruct):

        class Opcode(Enum):
            query = 0
            iquery = 1
            status = 2

        class ResponseCode(Enum):
            no_error = 0
            format_error = 1
            server_failure = 2
            name_error = 3
            not_implemented = 4
            refused = 5
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flag_response = self._io.read_bits_int_be(1) != 0
            self.opcode = self._io.read_bits_int_be(4)
            self.flag_authoritative = self._io.read_bits_int_be(1) != 0
            self.flag_truncated = self._io.read_bits_int_be(1) != 0
            self.flag_recursion_desired = self._io.read_bits_int_be(1) != 0
            self.flag_recursion_available = self._io.read_bits_int_be(1) != 0
            self.reserved = self._io.read_bits_int_be(3)
            self.response_code = self._io.read_bits_int_be(4)


    class Label(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            if self.length >= 192:
                self.pointer = self._io.read_u1()

            if self.length < 192:
                self.name = (self._io.read_bytes(self.length)).decode(u"ASCII")



    class DomainName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.labels = []
            i = 0
            while True:
                _ = DnsPacket.Label(self._io, self, self._root)
                self.labels.append(_)
                if  ((_.length == 0) or (_.length >= 192)) :
                    break
                i += 1


    class RdataA(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ip_addr = self._io.read_bytes(4)


    class RdataSoa(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mname = DnsPacket.DomainName(self._io, self, self._root)
            self.rname = DnsPacket.DomainName(self._io, self, self._root)
            self.serial = self._io.read_u4be()
            self.refresh = self._io.read_u4be()
            self.retry = self._io.read_u4be()
            self.expire = self._io.read_u4be()
            self.minimum = self._io.read_u4be()


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.id = self._io.read_u2be()
            self.flags = DnsPacket.Flags(self._io, self, self._root)
            self.qdcount = self._io.read_u2be()
            self.ancount = self._io.read_u2be()
            self.nscount = self._io.read_u2be()
            self.arcount = self._io.read_u2be()


    class RdataUnknown(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class RdataMx(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.preference = self._io.read_u2be()
            self.exchange = DnsPacket.DomainName(self._io, self, self._root)



