# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """National Imagery Transmission Format (NITF) 2.1 File Format."""
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.file_header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        i = 0
        while not self._io.is_eof():
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))
            i += 1


    class ImageHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.itype = (self._io.read_bytes(3)).decode(u"ASCII")
            self.irep = (self._io.read_bytes(8)).decode(u"ASCII")
            self.icat = (self._io.read_bytes(3)).decode(u"ASCII")
            self.nbands = self._io.read_u2be()
            self.imode = (self._io.read_bytes(1)).decode(u"ASCII")
            self.nbpc = self._io.read_u1()
            self.nppbh = self._io.read_u2be()
            self.nppbv = self._io.read_u2be()
            self.nbpp = self._io.read_u1()
            self.idlvl = self._io.read_u2be()
            self.ialvl = self._io.read_u2be()
            self.iloc = (self._io.read_bytes(10)).decode(u"ASCII")
            self.imag = (self._io.read_bytes(4)).decode(u"ASCII")
            self.geolocation = Nitf.GeolocationBlock(self._io, self, self._root)


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_header = Nitf.ImageHeader(self._io, self, self._root)
            self.image_data = self._io.read_bytes_full()


    class EncryptionBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.encryption_type = self._io.read_u1()
            self.encryption_key = (self._io.read_bytes(32)).decode(u"ASCII")


    class Coordinate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.lat = self._io.read_f8be()
            self.lon = self._io.read_f8be()


    class SecurityBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.country_code = (self._io.read_bytes(2)).decode(u"ASCII")
            self.release_instructions = (self._io.read_bytes(20)).decode(u"ASCII")


    class GeolocationBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.coords = []
            for i in range(4):
                self.coords.append(Nitf.Coordinate(self._io, self, self._root))



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
            self.fver = (self._io.read_bytes(5)).decode(u"ASCII")
            self.clevel = self._io.read_u1()
            self.stype = (self._io.read_bytes(10)).decode(u"ASCII")
            self.ostaid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.fdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.security = Nitf.SecurityBlock(self._io, self, self._root)
            self.fscop = self._io.read_u2be()
            self.fscpys = self._io.read_u2be()
            self.encryption = Nitf.EncryptionBlock(self._io, self, self._root)
            self.background_color = (self._io.read_bytes(3)).decode(u"ASCII")



