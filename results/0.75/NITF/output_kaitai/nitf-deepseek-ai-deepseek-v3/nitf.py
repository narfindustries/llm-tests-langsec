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
        for i in range(self.file_header.onum):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(self.file_header.tnum):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.file_header.tnum):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.file_header.enum):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.file_header.rnum):
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))

        self.end_of_file = Nitf.EndOfFile(self._io, self, self._root)

    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reid = (self._io.read_bytes(25)).decode(u"UTF-8")
            self.rever = self._io.read_u1()
            self.reoflw = self._io.read_u1()
            self.reitem = (self._io.read_bytes(3)).decode(u"UTF-8")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.iid = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.idatim = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.ititle = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.isorce = (self._io.read_bytes(42)).decode(u"UTF-8")
            self.nrows = self._io.read_u4be()
            self.ncols = self._io.read_u4be()
            self.pvtype = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.irep = (self._io.read_bytes(8)).decode(u"UTF-8")
            self.icat = (self._io.read_bytes(8)).decode(u"UTF-8")


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.textid = (self._io.read_bytes(7)).decode(u"UTF-8")
            self.txtalvl = self._io.read_u1()
            self.txtdt = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.txtitl = (self._io.read_bytes(80)).decode(u"UTF-8")


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.desid = (self._io.read_bytes(25)).decode(u"UTF-8")
            self.desver = self._io.read_u1()
            self.desoflw = self._io.read_u1()
            self.desitem = (self._io.read_bytes(3)).decode(u"UTF-8")


    class EndOfFile(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.eof = (self._io.read_bytes(3)).decode(u"UTF-8")


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sid = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.sname = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.scolor = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.sxorig = self._io.read_u4be()
            self.syorig = self._io.read_u4be()


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fileid = (self._io.read_bytes(9)).decode(u"UTF-8")
            self.fhdr = self._io.read_u2be()
            self.clevel = self._io.read_u1()
            self.stype = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.ostaid = (self._io.read_bytes(10)).decode(u"UTF-8")
            self.dt = (self._io.read_bytes(14)).decode(u"UTF-8")
            self.ftitle = (self._io.read_bytes(80)).decode(u"UTF-8")
            self.onum = self._io.read_u2be()
            self.tnum = self._io.read_u2be()
            self.tsize = self._io.read_u4be()
            self.enum = self._io.read_u2be()
            self.rnum = self._io.read_u2be()



