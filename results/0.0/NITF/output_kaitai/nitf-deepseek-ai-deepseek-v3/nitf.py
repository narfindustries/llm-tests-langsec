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

    class TextSegmentHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.te = (self._io.read_bytes(2)).decode(u"ASCII")
            self.textid = (self._io.read_bytes(7)).decode(u"ASCII")
            self.txtalvl = (self._io.read_bytes(3)).decode(u"ASCII")
            self.txtdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.txtitl = (self._io.read_bytes(80)).decode(u"ASCII")
            self.txsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.txscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txsctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txsrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.txsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.txsdcxm = (self._io.read_bytes(20)).decode(u"ASCII")
            self.txsdg = (self._io.read_bytes(20)).decode(u"ASCII")
            self.txsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.txscltx = (self._io.read_bytes(40)).decode(u"ASCII")


    class ImageSegmentHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = (self._io.read_bytes(2)).decode(u"ASCII")
            self.iid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.idatim = (self._io.read_bytes(14)).decode(u"ASCII")
            self.tgtid = (self._io.read_bytes(17)).decode(u"ASCII")
            self.isclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.iscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isdcxm = (self._io.read_bytes(20)).decode(u"ASCII")
            self.isdg = (self._io.read_bytes(20)).decode(u"ASCII")
            self.isdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.iscltx = (self._io.read_bytes(40)).decode(u"ASCII")
            self.encrypt = (self._io.read_bytes(1)).decode(u"ASCII")
            self.isorce = (self._io.read_bytes(42)).decode(u"ASCII")
            self.nrows = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ncols = (self._io.read_bytes(8)).decode(u"ASCII")
            self.pvtype = (self._io.read_bytes(3)).decode(u"ASCII")
            self.irep = (self._io.read_bytes(8)).decode(u"ASCII")
            self.icat = (self._io.read_bytes(8)).decode(u"ASCII")
            self.abpp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.pjust = (self._io.read_bytes(1)).decode(u"ASCII")
            self.icords = (self._io.read_bytes(1)).decode(u"ASCII")


    class DataExtensionSegmentHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.de = (self._io.read_bytes(2)).decode(u"ASCII")
            self.desid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.desver = (self._io.read_bytes(2)).decode(u"ASCII")
            self.desclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.descode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.desctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.desrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.desdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.desdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.desdcxm = (self._io.read_bytes(20)).decode(u"ASCII")
            self.desdg = (self._io.read_bytes(20)).decode(u"ASCII")
            self.desdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.descltx = (self._io.read_bytes(40)).decode(u"ASCII")


    class GraphicSegmentHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.sid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sdatim = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ssclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.ssdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ssdcxm = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssdg = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.sscltx = (self._io.read_bytes(40)).decode(u"ASCII")


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
            self.fscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsdcxm = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsdg = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fscltx = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscop = (self._io.read_bytes(5)).decode(u"ASCII")
            self.fscpys = (self._io.read_bytes(5)).decode(u"ASCII")
            self.oname = (self._io.read_bytes(24)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")



