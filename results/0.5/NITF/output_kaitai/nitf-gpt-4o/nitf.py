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
            self.fhdr = (self._io.read_bytes(9)).decode(u"ASCII")
            self.clevel = (self._io.read_bytes(2)).decode(u"ASCII")
            self.stype = (self._io.read_bytes(4)).decode(u"ASCII")
            self.ostaid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.fdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.fsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fscode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fsrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscaut = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsctln = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsdwng = (self._io.read_bytes(6)).decode(u"ASCII")
            self.fsdwngdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsdevt = (self._io.read_bytes(40)).decode(u"ASCII")
            self.oname = (self._io.read_bytes(24)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.fl = self._io.read_u4be()
            self.hl = self._io.read_u4be()
            self.numi = self._io.read_u2be()
            self.lish = self._io.read_u4be()
            self.li = []
            for i in range(self.numi):
                self.li.append(self._io.read_u4be())

            self.nums = self._io.read_u2be()
            self.lssh = self._io.read_u4be()
            self.ls = []
            for i in range(self.nums):
                self.ls.append(self._io.read_u4be())

            self.numt = self._io.read_u2be()
            self.ltsh = self._io.read_u4be()
            self.lt = []
            for i in range(self.numt):
                self.lt.append(self._io.read_u4be())

            self.numdes = self._io.read_u2be()
            self.ldesh = self._io.read_u4be()
            self.lde = []
            for i in range(self.numdes):
                self.lde.append(self._io.read_u4be())

            self.numres = self._io.read_u2be()
            self.lresh = self._io.read_u4be()
            self.lre = []
            for i in range(self.numres):
                self.lre.append(self._io.read_u4be())



    class ImageSubheader(KaitaiStruct):
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
            self.iscauth = (self._io.read_bytes(20)).decode(u"ASCII")
            self.isctln = (self._io.read_bytes(20)).decode(u"ASCII")
            self.isdwng = (self._io.read_bytes(6)).decode(u"ASCII")
            self.isdwngdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isdevt = (self._io.read_bytes(40)).decode(u"ASCII")
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
            self.nicom = self._io.read_u2be()
            self.icom = []
            for i in range(self.nicom):
                self.icom.append((self._io.read_bytes(80)).decode(u"ASCII"))



    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.te = (self._io.read_bytes(2)).decode(u"ASCII")
            self.textid = (self._io.read_bytes(80)).decode(u"ASCII")
            self.txtalvl = (self._io.read_bytes(1)).decode(u"ASCII")
            self.txtcode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txtctlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txtrel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.txtauth = (self._io.read_bytes(20)).decode(u"ASCII")
            self.txtctln = (self._io.read_bytes(20)).decode(u"ASCII")
            self.txtdwng = (self._io.read_bytes(6)).decode(u"ASCII")
            self.txtdwngdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.txtdevt = (self._io.read_bytes(40)).decode(u"ASCII")


    class DataExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.de = (self._io.read_bytes(2)).decode(u"ASCII")
            self.desid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.desver = self._io.read_u2be()
            self.declas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.decode = (self._io.read_bytes(40)).decode(u"ASCII")
            self.dectlh = (self._io.read_bytes(40)).decode(u"ASCII")
            self.derel = (self._io.read_bytes(40)).decode(u"ASCII")
            self.decaut = (self._io.read_bytes(20)).decode(u"ASCII")
            self.dectln = (self._io.read_bytes(20)).decode(u"ASCII")
            self.dedwng = (self._io.read_bytes(6)).decode(u"ASCII")
            self.dedwngdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.dedevt = (self._io.read_bytes(40)).decode(u"ASCII")



