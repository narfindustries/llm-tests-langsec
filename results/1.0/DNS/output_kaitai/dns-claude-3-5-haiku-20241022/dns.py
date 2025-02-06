# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Dns(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.transaction_id = self._io.read_u2be()
        self.flags = Dns.FlagsStruct(self._io, self, self._root)
        self.qdcount = self._io.read_u2be()
        self.ancount = self._io.read_u2be()
        self.nscount = self._io.read_u2be()
        self.arcount = self._io.read_u2be()
        self.questions = []
        for i in range(self.qdcount):
            self.questions.append(Dns.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.ancount):
            self.answers.append(Dns.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.nscount):
            self.authorities.append(Dns.ResourceRecord(self._io, self, self._root))

        self.additional = []
        for i in range(self.arcount):
            self.additional.append(Dns.ResourceRecord(self._io, self, self._root))


    class SoaRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.primary_ns = Dns.DomainName(self._io, self, self._root)
            self.responsible_person = Dns.DomainName(self._io, self, self._root)
            self.serial = self._io.read_u4be()
            self.refresh = self._io.read_s4be()
            self.retry = self._io.read_s4be()
            self.expire = self._io.read_s4be()
            self.minimum = self._io.read_u4be()


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = self._io.read_u2be()
            self.class = self._io.read_u2be()


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = self._io.read_u2be()
            self.class = self._io.read_u2be()
            self.ttl = self._io.read_s4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == 6:
                self.rdata = Dns.SoaRecord(self._io, self, self._root)
            elif _on == 1:
                self.rdata = Dns.ARecord(self._io, self, self._root)
            elif _on == 12:
                self.rdata = Dns.PtrRecord(self._io, self, self._root)
            elif _on == 5:
                self.rdata = Dns.CnameRecord(self._io, self, self._root)
            elif _on == 15:
                self.rdata = Dns.MxRecord(self._io, self, self._root)
            elif _on == 28:
                self.rdata = Dns.AaaaRecord(self._io, self, self._root)
            elif _on == 2:
                self.rdata = Dns.NsRecord(self._io, self, self._root)


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
            self.name = Dns.DomainName(self._io, self, self._root)


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
                _ = Dns.DomainName.DomainPart(self._io, self, self._root)
                self.parts.append(_)
                if _.length == 0:
                    break
                i += 1

        class DomainPart(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.length = self._io.read_u1()
                if self.length > 0:
                    self.part = (self._io.read_bytes(self.length)).decode(u"ascii")




    class PtrRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)


    class MxRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.preference = self._io.read_u2be()
            self.exchange = Dns.DomainName(self._io, self, self._root)


    class NsRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)



