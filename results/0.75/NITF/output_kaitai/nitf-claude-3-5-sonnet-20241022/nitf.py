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
        self.file_header = Nitf.FileHeaderType(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.file_header.numi):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(self.file_header.nums):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.file_header.numt):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.file_header.numdes):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.file_header.numres):
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))


    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_subheader = self._io.read_bytes(int(self._parent.file_header.lresh[i]))
            self.res_data = self._io.read_bytes(int(self._parent.file_header.lre[i]))


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(int(self.im.image_length))


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader = self._io.read_bytes(int(self._parent.file_header.ltsh[i]))
            self.text_data = self._io.read_bytes(int(self._parent.file_header.lt[i]))


    class FileHeaderType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = (self._io.read_bytes(4)).decode(u"ascii")
            self.fver = (self._io.read_bytes(5)).decode(u"ascii")
            self.clevel = (self._io.read_bytes(2)).decode(u"ascii")
            self.stype = (self._io.read_bytes(4)).decode(u"ascii")
            self.ostaid = (self._io.read_bytes(10)).decode(u"ascii")
            self.fdt = (self._io.read_bytes(14)).decode(u"ascii")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ascii")
            self.fsclas = (self._io.read_bytes(1)).decode(u"ascii")
            self.fsclsy = (self._io.read_bytes(2)).decode(u"ascii")
            self.fscode = (self._io.read_bytes(11)).decode(u"ascii")
            self.fsctlh = (self._io.read_bytes(2)).decode(u"ascii")
            self.fsrel = (self._io.read_bytes(20)).decode(u"ascii")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"ascii")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.fsdcxm = (self._io.read_bytes(4)).decode(u"ascii")
            self.fsdg = (self._io.read_bytes(1)).decode(u"ascii")
            self.fsdgdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.fscltx = (self._io.read_bytes(43)).decode(u"ascii")
            self.fscatp = (self._io.read_bytes(1)).decode(u"ascii")
            self.fscaut = (self._io.read_bytes(40)).decode(u"ascii")
            self.fscrsn = (self._io.read_bytes(1)).decode(u"ascii")
            self.fssrdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.fsctln = (self._io.read_bytes(15)).decode(u"ascii")
            self.fscop = (self._io.read_bytes(5)).decode(u"ascii")
            self.fscpys = (self._io.read_bytes(5)).decode(u"ascii")
            self.encryp = (self._io.read_bytes(1)).decode(u"ascii")
            self.fbkgc = (self._io.read_bytes(3)).decode(u"ascii")
            self.oname = (self._io.read_bytes(24)).decode(u"ascii")
            self.ophone = (self._io.read_bytes(18)).decode(u"ascii")
            self.fl = (self._io.read_bytes(12)).decode(u"ascii")
            self.hl = (self._io.read_bytes(6)).decode(u"ascii")
            self.numi = self._io.read_u4be()
            self.lish = []
            for i in range(self.numi):
                self.lish.append((self._io.read_bytes(6)).decode(u"ascii"))

            self.li = []
            for i in range(self.numi):
                self.li.append((self._io.read_bytes(10)).decode(u"ascii"))

            self.nums = self._io.read_u4be()
            self.lssh = []
            for i in range(self.nums):
                self.lssh.append((self._io.read_bytes(4)).decode(u"ascii"))

            self.ls = []
            for i in range(self.nums):
                self.ls.append((self._io.read_bytes(6)).decode(u"ascii"))

            self.numt = self._io.read_u4be()
            self.ltsh = []
            for i in range(self.numt):
                self.ltsh.append((self._io.read_bytes(4)).decode(u"ascii"))

            self.lt = []
            for i in range(self.numt):
                self.lt.append((self._io.read_bytes(5)).decode(u"ascii"))

            self.numdes = self._io.read_u4be()
            self.ldsh = []
            for i in range(self.numdes):
                self.ldsh.append((self._io.read_bytes(4)).decode(u"ascii"))

            self.ld = []
            for i in range(self.numdes):
                self.ld.append((self._io.read_bytes(9)).decode(u"ascii"))

            self.numres = self._io.read_u4be()
            self.lresh = []
            for i in range(self.numres):
                self.lresh.append((self._io.read_bytes(4)).decode(u"ascii"))

            self.lre = []
            for i in range(self.numres):
                self.lre.append((self._io.read_bytes(7)).decode(u"ascii"))

            self.udhdl = (self._io.read_bytes(5)).decode(u"ascii")
            if int(self.udhdl) > 0:
                self.udhofl = (self._io.read_bytes(3)).decode(u"ascii")

            if int(self.udhdl) > 0:
                self.udhd = self._io.read_bytes((int(self.udhdl) - 3))

            self.xhdl = (self._io.read_bytes(5)).decode(u"ascii")
            if int(self.xhdl) > 0:
                self.xhdlofl = (self._io.read_bytes(3)).decode(u"ascii")

            if int(self.xhdl) > 0:
                self.xhd = self._io.read_bytes((int(self.xhdl) - 3))



    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.des_subheader = self._io.read_bytes(int(self._parent.file_header.ldsh[i]))
            self.des_data = self._io.read_bytes(int(self._parent.file_header.ld[i]))


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = (self._io.read_bytes(2)).decode(u"ascii")
            self.iid1 = (self._io.read_bytes(10)).decode(u"ascii")
            self.idatim = (self._io.read_bytes(14)).decode(u"ascii")
            self.tgtid = (self._io.read_bytes(17)).decode(u"ascii")
            self.iid2 = (self._io.read_bytes(80)).decode(u"ascii")
            self.isclas = (self._io.read_bytes(1)).decode(u"ascii")
            self.isclsy = (self._io.read_bytes(2)).decode(u"ascii")
            self.iscode = (self._io.read_bytes(11)).decode(u"ascii")
            self.isctlh = (self._io.read_bytes(2)).decode(u"ascii")
            self.isrel = (self._io.read_bytes(20)).decode(u"ascii")
            self.isdctp = (self._io.read_bytes(2)).decode(u"ascii")
            self.isdcdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.isdcxm = (self._io.read_bytes(4)).decode(u"ascii")
            self.isdg = (self._io.read_bytes(1)).decode(u"ascii")
            self.isdgdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.iscltx = (self._io.read_bytes(43)).decode(u"ascii")
            self.iscatp = (self._io.read_bytes(1)).decode(u"ascii")
            self.iscaut = (self._io.read_bytes(40)).decode(u"ascii")
            self.iscrsn = (self._io.read_bytes(1)).decode(u"ascii")
            self.issrdt = (self._io.read_bytes(8)).decode(u"ascii")
            self.isctln = (self._io.read_bytes(15)).decode(u"ascii")
            self.encryp = (self._io.read_bytes(1)).decode(u"ascii")
            self.isorce = (self._io.read_bytes(42)).decode(u"ascii")
            self.nrows = (self._io.read_bytes(8)).decode(u"ascii")
            self.ncols = (self._io.read_bytes(8)).decode(u"ascii")
            self.pvtype = (self._io.read_bytes(3)).decode(u"ascii")
            self.irep = (self._io.read_bytes(8)).decode(u"ascii")
            self.icat = (self._io.read_bytes(8)).decode(u"ascii")
            self.abpp = (self._io.read_bytes(2)).decode(u"ascii")
            self.pjust = (self._io.read_bytes(1)).decode(u"ascii")
            self.icords = (self._io.read_bytes(1)).decode(u"ascii")
            if self.icords != u" ":
                self.igeolo = (self._io.read_bytes(60)).decode(u"ascii")

            self.nicom = (self._io.read_bytes(1)).decode(u"ascii")
            self.icom = []
            for i in range(int(self.nicom)):
                self.icom.append((self._io.read_bytes(80)).decode(u"ascii"))

            self.ic = (self._io.read_bytes(2)).decode(u"ascii")
            if self.ic != u"NC":
                self.comrat = (self._io.read_bytes(4)).decode(u"ascii")

            self.nbands = (self._io.read_bytes(1)).decode(u"ascii")
            if self.nbands == u"0":
                self.xbands = (self._io.read_bytes(5)).decode(u"ascii")

            self.irepband = []
            for i in range(int(self.nbands)):
                self.irepband.append((self._io.read_bytes(2)).decode(u"ascii"))

            self.isubcat = []
            for i in range(int(self.nbands)):
                self.isubcat.append((self._io.read_bytes(6)).decode(u"ascii"))

            self.ifc = []
            for i in range(int(self.nbands)):
                self.ifc.append((self._io.read_bytes(1)).decode(u"ascii"))

            self.imflt = []
            for i in range(int(self.nbands)):
                self.imflt.append((self._io.read_bytes(3)).decode(u"ascii"))

            self.nluts = []
            for i in range(int(self.nbands)):
                self.nluts.append((self._io.read_bytes(1)).decode(u"ascii"))

            self.nelut = []
            for i in range(int(self.nbands)):
                self.nelut.append((self._io.read_bytes(5)).decode(u"ascii"))

            if int(self.nluts[0]) > 0:
                self.lutd = []
                for i in range((int(self.nbands) * int(self.nelut[i]))):
                    self.lutd.append(self._io.read_bytes(1))


            self.isync = (self._io.read_bytes(1)).decode(u"ascii")
            self.imode = (self._io.read_bytes(1)).decode(u"ascii")
            self.nbpr = (self._io.read_bytes(4)).decode(u"ascii")
            self.nbpc = (self._io.read_bytes(4)).decode(u"ascii")
            self.nppbh = (self._io.read_bytes(4)).decode(u"ascii")
            self.nppbv = (self._io.read_bytes(4)).decode(u"ascii")
            self.nbpp = (self._io.read_bytes(2)).decode(u"ascii")
            self.idlvl = (self._io.read_bytes(3)).decode(u"ascii")
            self.ialvl = (self._io.read_bytes(3)).decode(u"ascii")
            self.iloc = (self._io.read_bytes(10)).decode(u"ascii")
            self.imag = (self._io.read_bytes(4)).decode(u"ascii")
            self.udidl = (self._io.read_bytes(5)).decode(u"ascii")
            if int(self.udidl) > 0:
                self.udofl = (self._io.read_bytes(3)).decode(u"ascii")

            if int(self.udidl) > 0:
                self.udid = self._io.read_bytes((int(self.udidl) - 3))

            self.ixshdl = (self._io.read_bytes(5)).decode(u"ascii")
            if int(self.ixshdl) > 0:
                self.ixsofl = (self._io.read_bytes(3)).decode(u"ascii")

            if int(self.ixshdl) > 0:
                self.ixshd = self._io.read_bytes((int(self.ixshdl) - 3))

            self.image_length = (self._io.read_bytes(10)).decode(u"ascii")


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_subheader = self._io.read_bytes(int(self._parent.file_header.lssh[i]))
            self.graphic_data = self._io.read_bytes(int(self._parent.file_header.ls[i]))



