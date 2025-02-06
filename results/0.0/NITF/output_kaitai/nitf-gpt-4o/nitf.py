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

    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.resid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.resver = (self._io.read_bytes(2)).decode(u"ASCII")
            self.reclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = (self._io.read_bytes(10)).decode(u"ASCII")
            self.iid1 = (self._io.read_bytes(80)).decode(u"ASCII")
            self.idatim = (self._io.read_bytes(14)).decode(u"ASCII")
            self.tgtid = (self._io.read_bytes(17)).decode(u"ASCII")
            self.iid2 = (self._io.read_bytes(80)).decode(u"ASCII")
            self.isclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.iscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isdcxm = (self._io.read_bytes(40)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.isorce = (self._io.read_bytes(42)).decode(u"ASCII")
            self.nrows = self._io.read_u4be()
            self.ncols = self._io.read_u4be()
            self.pvtype = (self._io.read_bytes(3)).decode(u"ASCII")
            self.irep = (self._io.read_bytes(8)).decode(u"ASCII")
            self.icat = (self._io.read_bytes(8)).decode(u"ASCII")
            self.abpp = self._io.read_u2be()
            self.pjust = (self._io.read_bytes(1)).decode(u"ASCII")
            self.icords = (self._io.read_bytes(1)).decode(u"ASCII")
            self.igeolo = (self._io.read_bytes(60)).decode(u"ASCII")


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.textid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.txtalvl = (self._io.read_bytes(1)).decode(u"ASCII")
            self.txtfmt = (self._io.read_bytes(3)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.desid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.desver = (self._io.read_bytes(2)).decode(u"ASCII")
            self.declas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sname = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.ssrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = (self._io.read_bytes(9)).decode(u"ASCII")
            self.clevel = (self._io.read_bytes(2)).decode(u"ASCII")
            self.stype = (self._io.read_bytes(4)).decode(u"ASCII")
            self.ostaid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.fdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.fsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsdcxm = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscop = (self._io.read_bytes(5)).decode(u"ASCII")
            self.fscpys = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.numi = self._io.read_u2be()
            self.numg = self._io.read_u2be()
            self.numt = self._io.read_u2be()
            self.numdes = self._io.read_u2be()
            self.numres = self._io.read_u2be()



