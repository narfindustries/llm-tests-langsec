# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Dns(KaitaiStruct):

    class RecordType(Enum):
        a = 1
        ns = 2
        cname = 5
        soa = 6
        ptr = 12
        mx = 15
        aaaa = 28

    class ClassType(Enum):
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
        self.question_count = self._io.read_u2be()
        self.answer_count = self._io.read_u2be()
        self.authority_count = self._io.read_u2be()
        self.additional_count = self._io.read_u2be()
        self.questions = []
        for i in range(self.question_count):
            self.questions.append(Dns.Question(self._io, self, self._root))

        self.answers = []
        for i in range(self.answer_count):
            self.answers.append(Dns.ResourceRecord(self._io, self, self._root))

        self.authorities = []
        for i in range(self.authority_count):
            self.authorities.append(Dns.ResourceRecord(self._io, self, self._root))

        self.additionals = []
        for i in range(self.additional_count):
            self.additionals.append(Dns.ResourceRecord(self._io, self, self._root))


    class Question(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(Dns.RecordType, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(Dns.ClassType, self._io.read_u2be())


    class Rdata(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self._raw_content = self._io.read_bytes_full()
            _io__raw_content = KaitaiStream(BytesIO(self._raw_content))
            self.content = Dns.ByteArray(_io__raw_content, self, self._root)


    class ResourceRecord(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Dns.DomainName(self._io, self, self._root)
            self.type = KaitaiStream.resolve_enum(Dns.RecordType, self._io.read_u2be())
            self.class = KaitaiStream.resolve_enum(Dns.ClassType, self._io.read_u2be())
            self.ttl = self._io.read_u4be()
            self.rdlength = self._io.read_u2be()
            self._raw_rdata = self._io.read_bytes(self.rdlength)
            _io__raw_rdata = KaitaiStream(BytesIO(self._raw_rdata))
            self.rdata = Dns.Rdata(_io__raw_rdata, self, self._root)


    class NamePart(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u1()
            if self.length >= 192:
                self.pointer = self._io.read_bits_int_be(2)

            self._io.align_to_byte()
            if  ((self.length < 192) and (self.length > 0)) :
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
                if  ((_.length == 0) or (_.pointer != 0)) :
                    break
                i += 1


    class ByteArray(KaitaiStruct):
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




