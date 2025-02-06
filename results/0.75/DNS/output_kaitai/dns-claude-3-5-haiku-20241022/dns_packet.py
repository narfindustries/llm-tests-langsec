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


    class SoaRecord(KaitaiStruct):
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


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)
            self.type = self._io.read_u2be()
            self.class = self._io.read_u2be()


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
            _on = self.type
            if _on == 6:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.SoaRecord(_io__raw_rdata, self, self._root)
            elif _on == 1:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.ARecord(_io__raw_rdata, self, self._root)
            elif _on == 12:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.PtrRecord(_io__raw_rdata, self, self._root)
            elif _on == 5:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.CnameRecord(_io__raw_rdata, self, self._root)
            elif _on == 15:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.MxRecord(_io__raw_rdata, self, self._root)
            elif _on == 28:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.AaaaRecord(_io__raw_rdata, self, self._root)
            elif _on == 2:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = DnsPacket.NsRecord(_io__raw_rdata, self, self._root)
            else:
                self.rdata = self._io.read_bytes(self.rdlength)


    class Flags(KaitaiStruct):
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
            self.z = self._io.read_bits_int_be(1) != 0
            self.rcode = self._io.read_bits_int_be(4)


    class AaaaRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(16):
                self.address.append(self._io.read_u1())



    class ARecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(4):
                self.address.append(self._io.read_u1())



    class CnameRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)


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
                _ = DnsPacket.DomainName.NamePart(self._io, self, self._root)
                self.name_parts.append(_)
                if _.length == 0:
                    break
                i += 1

        class NamePart(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.length = self._io.read_u1()
                if self.length != 0:
                    self.part = (self._io.read_bytes(self.length)).decode(u"ASCII")




    class PtrRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)


    class MxRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.preference = self._io.read_u2be()
            self.exchange = DnsPacket.DomainName(self._io, self, self._root)


    class NsRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = DnsPacket.DomainName(self._io, self, self._root)


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transaction_id = self._io.read_u2be()
            self.flags = DnsPacket.Flags(self._io, self, self._root)
            self.qdcount = self._io.read_u2be()
            self.ancount = self._io.read_u2be()
            self.nscount = self._io.read_u2be()
            self.arcount = self._io.read_u2be()



