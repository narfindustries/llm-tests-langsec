# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class DicomDeepseekChat(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.preamble = self._io.read_bytes(128)
        self.prefix = self._io.read_bytes(4)
        if not self.prefix == b"\x44\x49\x43\x4D":
            raise kaitaistruct.ValidationNotEqualError(b"\x44\x49\x43\x4D", self.prefix, self._io, u"/seq/1")
        self.data_elements = []
        i = 0
        while not self._io.is_eof():
            self.data_elements.append(DicomDeepseekChat.DataElement(self._io, self, self._root))
            i += 1


    class DataElement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u2le()
            self.vr = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.length = self._io.read_u2le()
            self.value = self._io.read_bytes(self.length)



