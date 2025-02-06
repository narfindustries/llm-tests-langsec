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
        self.image_segments = []
        for i in range(self.file_header.numi):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

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
            self.resid = (self._io.read_bytes(25)).decode(u"UTF-8")
            self.resver = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.reclas = (self._io.read_bytes(1)).decode(u"UTF-8")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.iid1 = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.idatim = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.tgtid = (self._io.read_bytes(17)).decode(u"UTF-8")
            self.iid2 = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.isorce = (self._io.read_bytes(42)).decode(u"UTF-8")
            self.icat = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.imag = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.igeolo = (self._io.read_bytes(60)).decode(u"UTF-8")
            self.ic = (self._io.read_bytes(2)).decode(u"UTF-8")


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.textid = (self._io.read_bytes(7)).decode(u"UTF-8")
            self.txtalvl = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.txtfmt = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.txtitl = (self._io.read_bytes(80)).decode(u"UTF-8")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.desid = (self._io.read_bytes(25)).decode(u"UTF-8")
            self.desver = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.declas = (self._io.read_bytes(1)).decode(u"UTF-8")


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.fver = (self._io.read_bytes(5)).decode(u"UTF-8")
            self.clevel = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.stype = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.ostaid = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.fdt = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.fsclas = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.fsclsy = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.fscode = (self._io.read_bytes(11)).decode(u"UTF-8")
            self.fsctlh = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.fsrel = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.fsdcxm = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.fsdg = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.fsdgdt = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.fscltx = (self._io.read_bytes(43)).decode(u"UTF-8")
            self.fscatp = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.fl = (self._io.read_bytes(12)).decode(u"UTF-8")
            self.hl = (self._io.read_bytes(6)).decode(u"UTF-8")
            self.numi = self._io.read_u4be()
            self.li = []
            for i in range(self.numi):
                self.li.append(self._io.read_u4be())

            self.numt = self._io.read_u4be()
            self.lt = []
            for i in range(self.numt):
                self.lt.append(self._io.read_u4be())

            self.numdes = self._io.read_u4be()
            self.ldes = []
            for i in range(self.numdes):
                self.ldes.append(self._io.read_u4be())

            self.numres = self._io.read_u4be()
            self.lres = []
            for i in range(self.numres):
                self.lres.append(self._io.read_u4be())




