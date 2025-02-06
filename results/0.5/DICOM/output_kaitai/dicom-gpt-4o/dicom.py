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
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x44\x49\x43\x4D":
            raise kaitaistruct.ValidationNotEqualError(b"\x44\x49\x43\x4D", self.magic, self._io, u"/seq/1")
        self.elements = Dicom.DicomElements(self._io, self, self._root)

    class DicomElements(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.elements = []
            i = 0
            while not self._io.is_eof():
                self.elements.append(Dicom.DicomElement(self._io, self, self._root))
                i += 1



    class DicomElement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag_group = self._io.read_u2le()
            self.tag_element = self._io.read_u2le()
            if self.tag_group != 2:
                self.vr = (self._io.read_bytes(2)).decode(u"ASCII")

            if  ((self.tag_group != 2) and ( ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) )) :
                self.reserved = self._io.read_bytes(2)

            if  ((self.tag_group == 2) or ( ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) )) :
                self.value_length = self._io.read_u4le()

            if  ((self.tag_group != 2) and (not ( ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) ))) :
                self.value_length_short = self._io.read_u2le()

            if  ((self.tag_group == 2) or ( ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) )) :
                self.value = self._io.read_bytes(self.value_length)

            if  ((self.tag_group != 2) and (not ( ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) ))) :
                self.value_short = self._io.read_bytes(self.value_length_short)




