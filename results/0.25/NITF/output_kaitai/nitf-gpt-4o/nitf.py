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
            self.re = (self._io.read_bytes(2)).decode(u"ASCII")
            self.redata = self._io.read_bytes_full()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = (self._io.read_bytes(2)).decode(u"ASCII")
            self.iid1 = (self._io.read_bytes(10)).decode(u"ASCII")
            self.idatim = (self._io.read_bytes(14)).decode(u"ASCII")
            self.tgtid = (self._io.read_bytes(17)).decode(u"ASCII")
            self.iid2 = (self._io.read_bytes(80)).decode(u"ASCII")
            self.isclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.iscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.isdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.isdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.isdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.iscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.iscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.iscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.iscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.issrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isctln = (self._io.read_bytes(15)).decode(u"ASCII")
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
            self.nicom = self._io.read_u1()
            self.icom = []
            for i in range(self.nicom):
                self.icom.append((self._io.read_bytes(80)).decode(u"ASCII"))



    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.te = (self._io.read_bytes(2)).decode(u"ASCII")
            self.tid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.ttitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.tsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.tsctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.tsrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.tsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.tsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.tsdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.tsdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.tscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.tscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.tscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tssrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.tsctln = (self._io.read_bytes(15)).decode(u"ASCII")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.de = (self._io.read_bytes(2)).decode(u"ASCII")
            self.dedata = self._io.read_bytes_full()


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.sid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sname = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.ssdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.ssdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ssdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.ssdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.ssdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.sscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.sscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.sscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sssrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ssctln = (self._io.read_bytes(15)).decode(u"ASCII")


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
            self.fbkgd = (self._io.read_bytes(3)).decode(u"ASCII")
            self.oname = (self._io.read_bytes(24)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.fl = self._io.read_u4be()
            self.hl = self._io.read_u2be()
            self.numi = self._io.read_u2be()
            self.li = []
            for i in range(self.numi):
                self.li.append(self._io.read_u4be())

            self.numsi = self._io.read_u2be()
            self.lsi = []
            for i in range(self.numsi):
                self.lsi.append(self._io.read_u4be())

            self.numt = self._io.read_u2be()
            self.lt = []
            for i in range(self.numt):
                self.lt.append(self._io.read_u4be())

            self.numdes = self._io.read_u2be()
            self.ldes = []
            for i in range(self.numdes):
                self.ldes.append(self._io.read_u4be())

            self.numres = self._io.read_u2be()
            self.lres = []
            for i in range(self.numres):
                self.lres.append(self._io.read_u4be())

            self.udhdl = self._io.read_u4be()
            self.udhofl = self._io.read_u2be()
            self.udhd = self._io.read_bytes(self.udhdl)
            self.xhdl = self._io.read_u4be()
            self.xhdlof = self._io.read_u2be()
            self.xhd = self._io.read_bytes(self.xhdl)



