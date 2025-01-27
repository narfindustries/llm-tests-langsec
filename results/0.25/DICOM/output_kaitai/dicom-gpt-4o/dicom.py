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
        self.prefix = self._io.read_bytes(4)
        if not self.prefix == b"\x44\x49\x43\x4D":
            raise kaitaistruct.ValidationNotEqualError(b"\x44\x49\x43\x4D", self.prefix, self._io, u"/seq/1")
        self.elements = []
        i = 0
        while not self._io.is_eof():
            self.elements.append(Dicom.Element(self._io, self, self._root))
            i += 1


    class Element(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag_group = self._io.read_u2le()
            self.tag_element = self._io.read_u2le()
            self.vr = (self._io.read_bytes(2)).decode(u"utf-8")
            if self._parent.is_explicit_vr:
                self.reserved = self._io.read_bytes(2)

            if self._parent.is_explicit_vr:
                self.value_length = self._io.read_u4le()

            if not (self._parent.is_explicit_vr):
                self.value_length_implicit = self._io.read_u2le()

            if self._parent.is_explicit_vr:
                self.value = self._io.read_bytes(self.value_length)

            if not (self._parent.is_explicit_vr):
                self.value_implicit = self._io.read_bytes(self.value_length_implicit)



    @property
    def is_explicit_vr(self):
        if hasattr(self, '_m_is_explicit_vr'):
            return self._m_is_explicit_vr

        self._m_is_explicit_vr =  ((KaitaiStream.byte_array_index(self.preamble, 128) == 68) and (KaitaiStream.byte_array_index(self.preamble, 129) == 73) and (KaitaiStream.byte_array_index(self.preamble, 130) == 67) and (KaitaiStream.byte_array_index(self.preamble, 131) == 77)) 
        return getattr(self, '_m_is_explicit_vr', None)


