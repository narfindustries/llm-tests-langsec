# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DnsPacket(KaitaiStruct):

    class Type(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        ptr = 12
        mx = 15
        txt = 16
        aaaa = 28

    class Class(Enum):
        in = 1
        ch = 3
        hs = 4

    class Opcode(Enum):
        query = 0
        iquery = 1
        status = 2
        notify = 4
        update = 5

    class Rcode(Enum):
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
        self.transaction_id = self._io.read_u2be()
        self.flags = DnsPacket.PacketFlags(self._io, self, self._root)
        self.qdcount = self._io.read_u2be()
        self.ancount = self._io.read_u2be()
        self.nscount = self._io.read_u2be()
        self.arcount = self._io.read_u2be()
        self.questions = []
        for i in range(self.qdcount):
            self.questions.append(DnsPacket.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.ancount):
            self.answers.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.nscount):
            self.authorities.append(DnsPacket.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.arcount):
            self.additionals.append(DnsPacket.ResourceRecord(self._io, self, self._root))


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.Type, self._io.read_u2be())
            self.qclass = KaitaiStream.resolve_enum(DnsPacket.Class, self._io.read_u2be())


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.Type, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.Class, self._io.read_u2be())
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == DnsPacket.Type.ptr:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.ns:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.txt:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataTxt(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.a:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataA(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.mx:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataMx(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.aaaa:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataAaaa(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.soa:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RdataSoa(_io__raw_rdata, self, self._root)
            elif _on == DnsPacket.Type.cname:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.DomainName(_io__raw_rdata, self, self._root)
            else:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.RawRdata(_io__raw_rdata, self, self._root)


    class RdataAaaa(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ip_addr = self._io.read_bytes(16)


    class TxtString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            self.text = (self._io.read_bytes(self.length)).decode(u"ASCII")


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

            if  ((self.length > 0) and (self.length < 192)) :
                self.name = (self._io.read_bytes(self.length)).decode(u"ASCII")


        @property
        def is_pointer(self):
            if hasattr(self, '_m_is_pointer'):
                return self._m_is_pointer

            self._m_is_pointer = self.length >= 192
            return getattr(self, '_m_is_pointer', None)

        @property
        def offset(self):
            if hasattr(self, '_m_offset'):
                return self._m_offset

            if self.is_pointer:
                self._m_offset = (((self.length & 63) << 8) | self.pointer)

            return getattr(self, '_m_offset', None)


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
                if  ((_.length == 0) or (_.is_pointer)) :
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


    class RdataTxt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.strings = []
            i = 0
            while not self._io.is_eof():
                self.strings.append(DnsPacket.TxtString(self._io, self, self._root))
                i += 1



    class PacketFlags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qr = self._io.read_bits_int_be(1) != 0
            self.opcode = KaitaiStream.resolve_enum(DnsPacket.Opcode, self._io.read_bits_int_be(4))
            self.aa = self._io.read_bits_int_be(1) != 0
            self.tc = self._io.read_bits_int_be(1) != 0
            self.rd = self._io.read_bits_int_be(1) != 0
            self.ra = self._io.read_bits_int_be(1) != 0
            self.z = self._io.read_bits_int_be(3)
            self.rcode = KaitaiStream.resolve_enum(DnsPacket.Rcode, self._io.read_bits_int_be(4))


    class RawRdata(KaitaiStruct):
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



