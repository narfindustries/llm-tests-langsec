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
            self.file_type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.version = (self._io.read_bytes(5)).decode(u"ASCII")
            self.complexity_level = (self._io.read_bytes(2)).decode(u"ASCII")
            self.stype = (self._io.read_bytes(4)).decode(u"ASCII")
            self.odate = (self._io.read_bytes(14)).decode(u"ASCII")
            self.otime = (self._io.read_bytes(14)).decode(u"ASCII")
            self.file_title = (self._io.read_bytes(80)).decode(u"ASCII")
            self.file_security = Nitf.FileSecurity(self._io, self, self._root)


    class FileSecurity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification = (self._io.read_bytes(1)).decode(u"ASCII")
            self.classification_system = (self._io.read_bytes(2)).decode(u"ASCII")
            self.codewords = (self._io.read_bytes(11)).decode(u"ASCII")
            self.control_and_handling = (self._io.read_bytes(2)).decode(u"ASCII")
            self.release_instructions = (self._io.read_bytes(20)).decode(u"ASCII")
            self.declassification_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.declassification_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.declassification_exemption = (self._io.read_bytes(4)).decode(u"ASCII")
            self.downgrade = (self._io.read_bytes(1)).decode(u"ASCII")
            self.downgrade_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.classification_text = (self._io.read_bytes(43)).decode(u"ASCII")
            self.classification_authority_type = (self._io.read_bytes(1)).decode(u"ASCII")
            self.classification_authority = (self._io.read_bytes(40)).decode(u"ASCII")
            self.classification_reason = (self._io.read_bytes(1)).decode(u"ASCII")
            self.security_source_date = (self._io.read_bytes(8)).decode(u"ASCII")
            self.security_control_number = (self._io.read_bytes(15)).decode(u"ASCII")
            self.security_control_number_2 = (self._io.read_bytes(15)).decode(u"ASCII")



