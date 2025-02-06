# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.file_header = Nitf.FileHeader(self._io, self, self._root)

    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = (self._io.read_bytes(4)).decode(u"ASCII")
            self.fver = (self._io.read_bytes(5)).decode(u"ASCII")
            self.clevel = (self._io.read_bytes(2)).decode(u"ASCII")
            self.stype = (self._io.read_bytes(4)).decode(u"ASCII")
            self.ostaid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.fdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.fsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fscode = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsctlh = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsrel = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.fsdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.fscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fssrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsctln = (self._io.read_bytes(15)).decode(u"ASCII")
            self.fscop = (self._io.read_bytes(5)).decode(u"ASCII")
            self.fscpys = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fbgkc = (self._io.read_bytes(6)).decode(u"ASCII")
            self.oname = (self._io.read_bytes(24)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.fl = self._io.read_u8be()



