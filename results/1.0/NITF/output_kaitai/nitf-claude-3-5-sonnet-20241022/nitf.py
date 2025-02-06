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
        self.header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(int(self.header.numi)):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(int(self.header.nums)):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(int(self.header.numt)):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(int(self.header.numdes)):
            self.data_extension_segments.append(Nitf.DesSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(int(self.header.numres)):
            self.reserved_extension_segments.append(Nitf.ResSegment(self._io, self, self._root))


    class ImageBand(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.irepband = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isubcat = (self._io.read_bytes(6)).decode(u"ASCII")
            self.ifc = (self._io.read_bytes(1)).decode(u"ASCII")
            self.imflt = (self._io.read_bytes(3)).decode(u"ASCII")
            self.nluts = (self._io.read_bytes(1)).decode(u"ASCII")
            if self.nluts != u"0":
                self.nelut = (self._io.read_bytes(5)).decode(u"ASCII")



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
            self.isclsy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.iscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.isctlh = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isrel = (self._io.read_bytes(20)).decode(u"ASCII")
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
            self.encryp = self._io.read_u1()
            self.isorce = (self._io.read_bytes(42)).decode(u"ASCII")
            self.nrows = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ncols = (self._io.read_bytes(8)).decode(u"ASCII")
            self.pvtype = (self._io.read_bytes(3)).decode(u"ASCII")
            self.irep = (self._io.read_bytes(8)).decode(u"ASCII")
            self.icat = (self._io.read_bytes(8)).decode(u"ASCII")
            self.abpp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.pjust = (self._io.read_bytes(1)).decode(u"ASCII")
            self.icords = (self._io.read_bytes(1)).decode(u"ASCII")
            if self.icords != u" ":
                self.igeolo = (self._io.read_bytes(60)).decode(u"ASCII")

            self.nicom = (self._io.read_bytes(1)).decode(u"ASCII")
            self.ic = (self._io.read_bytes(2)).decode(u"ASCII")
            if self.ic != u"NC":
                self.comrat = (self._io.read_bytes(4)).decode(u"ASCII")

            self.nbands = (self._io.read_bytes(1)).decode(u"ASCII")
            if self.nbands == u"0":
                self.xbands = (self._io.read_bytes(5)).decode(u"ASCII")

            self.image_bands = []
            for i in range(int(self.nbands)):
                self.image_bands.append(Nitf.ImageBand(self._io, self, self._root))

            self.isync = (self._io.read_bytes(1)).decode(u"ASCII")
            self.imode = (self._io.read_bytes(1)).decode(u"ASCII")
            self.nbpr = (self._io.read_bytes(4)).decode(u"ASCII")
            self.nbpc = (self._io.read_bytes(4)).decode(u"ASCII")
            self.nppbh = (self._io.read_bytes(4)).decode(u"ASCII")
            self.nppbv = (self._io.read_bytes(4)).decode(u"ASCII")
            self.nbpp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.idlvl = (self._io.read_bytes(3)).decode(u"ASCII")
            self.ialvl = (self._io.read_bytes(3)).decode(u"ASCII")
            self.iloc = (self._io.read_bytes(10)).decode(u"ASCII")
            self.imag = (self._io.read_bytes(4)).decode(u"ASCII")
            self.udidl = (self._io.read_bytes(5)).decode(u"ASCII")
            if int(self.udidl) > 0:
                self.udofl = (self._io.read_bytes(3)).decode(u"ASCII")

            if int(self.udidl) > 0:
                self.udid = self._io.read_bytes((int(self.udidl) - 3))

            self.ixshdl = (self._io.read_bytes(5)).decode(u"ASCII")
            if int(self.ixshdl) > 0:
                self.ixsofl = (self._io.read_bytes(3)).decode(u"ASCII")

            if int(self.ixshdl) > 0:
                self.ixshd = self._io.read_bytes((int(self.ixshdl) - 3))



    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.te = (self._io.read_bytes(2)).decode(u"ASCII")
            self.textid = (self._io.read_bytes(7)).decode(u"ASCII")
            self.txtdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.txtitl = (self._io.read_bytes(80)).decode(u"ASCII")
            self.tsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tsclsy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.tscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.tsctlh = (self._io.read_bytes(2)).decode(u"ASCII")
            self.tsrel = (self._io.read_bytes(20)).decode(u"ASCII")
            self.tsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.tsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.tsdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.tsdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.tscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.tscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.tscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.tscrsn = (self._io.read_bytes(1)).decode(u"ASCII")


    class ResSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_data = self._io.read_bytes(1)


    class DesSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.des_data = self._io.read_bytes(1)


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
            self.ssclsy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.sscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.ssctlh = (self._io.read_bytes(2)).decode(u"ASCII")
            self.ssrel = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.ssdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.ssdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.ssdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.ssdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.sscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.sscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.sscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.ssctln = (self._io.read_bytes(15)).decode(u"ASCII")
            self.encryp = self._io.read_u1()
            self.sfmt = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sstruct = (self._io.read_bytes(13)).decode(u"ASCII")
            self.sdlvl = (self._io.read_bytes(3)).decode(u"ASCII")
            self.salvl = (self._io.read_bytes(3)).decode(u"ASCII")
            self.sloc = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sbnd1 = (self._io.read_bytes(10)).decode(u"ASCII")
            self.scolor = (self._io.read_bytes(1)).decode(u"ASCII")
            self.sbnd2 = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sres2 = (self._io.read_bytes(20)).decode(u"ASCII")
            self.sxshdl = (self._io.read_bytes(5)).decode(u"ASCII")
            if int(self.sxshdl) > 0:
                self.sxsofl = (self._io.read_bytes(3)).decode(u"ASCII")

            if int(self.sxshdl) > 0:
                self.sxshd = self._io.read_bytes((int(self.sxshdl) - 3))



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
            self.fsclsy = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.fsctlh = (self._io.read_bytes(2)).decode(u"ASCII")
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
            self.encryp = self._io.read_u1()
            self.fbkgc = self._io.read_bytes(3)
            self.oname = (self._io.read_bytes(24)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.fl = (self._io.read_bytes(12)).decode(u"ASCII")
            self.hl = (self._io.read_bytes(6)).decode(u"ASCII")
            self.numi = (self._io.read_bytes(3)).decode(u"ASCII")
            self.nums = (self._io.read_bytes(3)).decode(u"ASCII")
            self.numt = (self._io.read_bytes(3)).decode(u"ASCII")
            self.numdes = (self._io.read_bytes(3)).decode(u"ASCII")
            self.numres = (self._io.read_bytes(3)).decode(u"ASCII")
            self.udhdl = (self._io.read_bytes(5)).decode(u"ASCII")
            if int(self.udhdl) > 0:
                self.udhofl = (self._io.read_bytes(3)).decode(u"ASCII")

            if int(self.udhdl) > 0:
                self.udhd = self._io.read_bytes((int(self.udhdl) - 3))

            self.xhdl = (self._io.read_bytes(5)).decode(u"ASCII")
            if int(self.xhdl) > 0:
                self.xhdlofl = (self._io.read_bytes(3)).decode(u"ASCII")

            if int(self.xhdl) > 0:
                self.xhd = self._io.read_bytes((int(self.xhdl) - 3))




