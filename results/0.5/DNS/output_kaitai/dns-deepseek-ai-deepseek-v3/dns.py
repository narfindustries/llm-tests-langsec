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
        self.header = Dns.Header(self._io, self, self._root)
        self.questions = []
        for i in range(self.header.qdcount):
            self.questions.append(Dns.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.header.ancount):
            self.answers.append(Dns.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.header.nscount):
            self.authorities.append(Dns.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.header.arcount):
            self.additionals.append(Dns.ResourceRecord(self._io, self, self._root))


    class SoaRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.mname = Dns.DomainName(self._io, self, self._root)
            self.rname = Dns.DomainName(self._io, self, self._root)
            self.serial = self._io.read_u4be()
            self.refresh = self._io.read_u4be()
            self.retry = self._io.read_u4be()
            self.expire = self._io.read_u4be()
            self.minimum = self._io.read_u4be()


    class TxtRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.texts = []
            i = 0
            while True:
                _ = Dns.TxtRecord.TxtData(self._io, self, self._root)
                self.texts.append(_)
                if _.length == 0:
                    break
                i += 1

        class TxtData(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.length = self._io.read_u1()
                self.text = (self._io.read_bytes(self.length)).decode(u"UTF-8")



    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.qname = Dns.DomainName(self._io, self, self._root)
            self.qtype = self._io.read_u2be()
            self.qclass = self._io.read_u2be()


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
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            _on = self.type
            if _on == 6:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.SoaRecord(_io__raw_rdata, self, self._root)
            elif _on == 1:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.Ipv4Address(_io__raw_rdata, self, self._root)
            elif _on == 12:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.DomainName(_io__raw_rdata, self, self._root)
            elif _on == 5:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.DomainName(_io__raw_rdata, self, self._root)
            elif _on == 15:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.MxRecord(_io__raw_rdata, self, self._root)
            elif _on == 28:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.Ipv6Address(_io__raw_rdata, self, self._root)
            elif _on == 16:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.TxtRecord(_io__raw_rdata, self, self._root)
            elif _on == 2:
                self._raw_rdata = self._io.read_bytes(self.rdlength)
                _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
                self.rdata = Dns.DomainName(_io__raw_rdata, self, self._root)
            else:
                self.rdata = self._io.read_bytes(self.rdlength)


    class Ipv6Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = []
            for i in range(16):
                self.address.append(self._io.read_u1())



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
                _ = Dns.DomainName.Label(self._io, self, self._root)
                self.labels.append(_)
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
                self.name = (self._io.read_bytes(self.length)).decode(u"UTF-8")



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



