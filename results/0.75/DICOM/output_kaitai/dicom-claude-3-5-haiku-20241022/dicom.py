# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Dicom(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.preamble = self._io.read_bytes(128)
        self.magic = (self._io.read_bytes(4)).decode(u"ASCII")
        self.file_meta_info_set = Dicom.MetaInfoSequence(self._io, self, self._root)

    class MetaInfoSequence(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group_length = self._io.read_u2le()
            self.transfer_syntax = Dicom.TagElement(self._io, self, self._root)
            self.implementation_uid = Dicom.TagElement(self._io, self, self._root)


    class TagElement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u4le()
            self.vr = (self._io.read_bytes(2)).decode(u"ASCII")
            self.length = self._io.read_u2le()
            self.value = (self._io.read_bytes(self.length)).decode(u"ASCII")



