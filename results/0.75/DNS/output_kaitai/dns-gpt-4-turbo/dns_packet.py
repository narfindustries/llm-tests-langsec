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
    the underlying network protocols. By providing a worldwide keyword-based redirection
    service, the Domain Name System is an essential component of the functionality
    of the Internet. This Kaitai Struct specification describes the structure of DNS packets.
    """

    class TypeType(Enum):
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

    class RcodeType(Enum):
        no_error = 0
        format_error = 1
        server_failure = 2
        name_error = 3
        not_implemented = 4
        refused = 5

    class ClassType(Enum):
        in = 1
        ch = 3
        hs = 4
        any = 255

    class OpcodeType(Enum):
        query = 0
        iquery = 1
        status = 2
        reserved = 3
        notify = 4
        update = 5

    class QrType(Enum):
        query = 0
        response = 1
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
            self.rdata = self._io.read_bytes(self.rdlength)


    class Label(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u1()
            if not self.len >= 1:
                raise kaitaistruct.ValidationLessThanError(1, self.len, self._io, u"/types/label/seq/0")
            self.name = (self._io.read_bytes(self.len)).decode(u"ASCII")


    class Query(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(DnsPacket.TypeType, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(DnsPacket.ClassType, self._io.read_u2be())


    class DomainName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.parts = []
            i = 0
            while not self._io.is_eof():
                self.parts.append(DnsPacket.Label(self._io, self, self._root))
                i += 1



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

            self._m_qr = KaitaiStream.resolve_enum(DnsPacket.QrType, (self.flags >> 15))
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

            self._m_rcode = KaitaiStream.resolve_enum(DnsPacket.RcodeType, (self.flags & 15))
            return getattr(self, '_m_rcode', None)

        @property
        def opcode(self):
            if hasattr(self, '_m_opcode'):
                return self._m_opcode

            self._m_opcode = KaitaiStream.resolve_enum(DnsPacket.OpcodeType, ((self.flags >> 11) & 15))
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



