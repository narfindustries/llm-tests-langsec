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
        for i in range(self.file_header.num_image_segments):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphic_segments = []
        for i in range(self.file_header.num_graphic_segments):
            self.graphic_segments.append(Nitf.GraphicSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.file_header.num_text_segments):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.extension_segments = []
        for i in range(self.file_header.num_extension_segments):
            self.extension_segments.append(Nitf.ExtensionSegment(self._io, self, self._root))


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tid = (self._io.read_bytes(10)).decode(u"utf-8")
            self.tdatim = (self._io.read_bytes(14)).decode(u"utf-8")
            self.ttitle = (self._io.read_bytes(80)).decode(u"utf-8")
            self.tsclas = (self._io.read_bytes(1)).decode(u"utf-8")
            if self.tsclas != u"U":
                self.tscocode = (self._io.read_bytes(2)).decode(u"utf-8")

            if self.tsclas != u"U":
                self.tsctlh = (self._io.read_bytes(20)).decode(u"utf-8")

            if self.tsclas != u"U":
                self.tsrel = (self._io.read_bytes(20)).decode(u"utf-8")

            self.data_length = self._io.read_u4be()


    class ExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.esubhdr = Nitf.ExtensionSubheader(self._io, self, self._root)
            self.extension_data = self._io.read_bytes(self.esubhdr.data_length)


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.isubhdr = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.isubhdr.data_length)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tsubhdr = Nitf.TextSubheader(self._io, self, self._root)
            self.text_data = self._io.read_bytes(self.tsubhdr.data_length)


    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gid = (self._io.read_bytes(10)).decode(u"utf-8")
            self.gdatim = (self._io.read_bytes(14)).decode(u"utf-8")
            self.gtitle = (self._io.read_bytes(80)).decode(u"utf-8")
            self.gsclas = (self._io.read_bytes(1)).decode(u"utf-8")
            if self.gsclas != u"U":
                self.gscocode = (self._io.read_bytes(2)).decode(u"utf-8")

            if self.gsclas != u"U":
                self.gsctlh = (self._io.read_bytes(20)).decode(u"utf-8")

            if self.gsclas != u"U":
                self.gsrel = (self._io.read_bytes(20)).decode(u"utf-8")

            self.data_length = self._io.read_u4be()


    class ExtensionSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.eid = (self._io.read_bytes(10)).decode(u"utf-8")
            self.edatim = (self._io.read_bytes(14)).decode(u"utf-8")
            self.etitle = (self._io.read_bytes(80)).decode(u"utf-8")
            self.esclas = (self._io.read_bytes(1)).decode(u"utf-8")
            if self.esclas != u"U":
                self.escocode = (self._io.read_bytes(2)).decode(u"utf-8")

            if self.esclas != u"U":
                self.esctlh = (self._io.read_bytes(20)).decode(u"utf-8")

            if self.esclas != u"U":
                self.esrel = (self._io.read_bytes(20)).decode(u"utf-8")

            self.data_length = self._io.read_u4be()


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.iid1 = (self._io.read_bytes(10)).decode(u"utf-8")
            self.idatim = (self._io.read_bytes(14)).decode(u"utf-8")
            self.ititle = (self._io.read_bytes(80)).decode(u"utf-8")
            self.isclas = (self._io.read_bytes(1)).decode(u"utf-8")
            if self.isclas != u"U":
                self.iscocode = (self._io.read_bytes(2)).decode(u"utf-8")

            if self.isclas != u"U":
                self.isctlh = (self._io.read_bytes(20)).decode(u"utf-8")

            if self.isclas != u"U":
                self.isrel = (self._io.read_bytes(20)).decode(u"utf-8")

            self.nbands = self._io.read_u1()
            self.icords = (self._io.read_bytes(1)).decode(u"utf-8")
            self.data_length = self._io.read_u4be()


    class GraphicSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gsubhdr = Nitf.GraphicSubheader(self._io, self, self._root)
            self.graphic_data = self._io.read_bytes(self.gsubhdr.data_length)


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = (self._io.read_bytes(4)).decode(u"utf-8")
            self.clevel = (self._io.read_bytes(2)).decode(u"utf-8")
            self.stype = (self._io.read_bytes(4)).decode(u"utf-8")
            self.ostaid = (self._io.read_bytes(10)).decode(u"utf-8")
            self.ftitle = (self._io.read_bytes(80)).decode(u"utf-8")
            self.fsclas = (self._io.read_bytes(1)).decode(u"utf-8")
            if self.fsclas != u"U":
                self.fscode = (self._io.read_bytes(2)).decode(u"utf-8")

            if self.fsclas != u"U":
                self.fsctlh = (self._io.read_bytes(20)).decode(u"utf-8")

            if self.fsclas != u"U":
                self.fsrel = (self._io.read_bytes(20)).decode(u"utf-8")

            self.encryp = (self._io.read_bytes(1)).decode(u"utf-8")
            self.ftxtd = self._io.read_u4be()
            self.num_image_segments = self._io.read_u2be()
            self.num_graphic_segments = self._io.read_u2be()
            self.num_text_segments = self._io.read_u2be()
            self.num_extension_segments = self._io.read_u2be()



