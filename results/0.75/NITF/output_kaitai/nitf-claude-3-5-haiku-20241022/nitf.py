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
        i = 0
        while not self._io.is_eof():
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))
            i += 1


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fhdr = self._io.read_bytes(4)
            if not self.fhdr == b"\x4E\x49\x54\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x4E\x49\x54\x46", self.fhdr, self._io, u"/types/file_header/seq/0")
            self.fver = self._io.read_bytes(2)
            self.clevel = self._io.read_bytes(2)
            self.stype = self._io.read_bytes(4)
            self.ostaid = self._io.read_bytes(10)
            self.fdt = self._io.read_bytes(14)
            self.ftitle = self._io.read_bytes(80)
            self.fpseudo = self._io.read_bytes(1)
            self.num_image_segments = self._io.read_u2be()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.header = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes_full()


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.im = self._io.read_bytes(2)
            if not self.im == b"\x49\x4D":
                raise kaitaistruct.ValidationNotEqualError(b"\x49\x4D", self.im, self._io, u"/types/image_subheader/seq/0")
            self.iid1 = self._io.read_bytes(10)
            self.idatim = self._io.read_bytes(14)
            self.tgt = self._io.read_bytes(17)
            self.iid2 = self._io.read_bytes(80)
            self.isclas = self._io.read_bytes(1)



