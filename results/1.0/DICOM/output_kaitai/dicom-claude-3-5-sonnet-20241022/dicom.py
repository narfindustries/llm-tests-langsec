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
        self.prefix = (self._io.read_bytes(4)).decode(u"ASCII")
        self.dataset = []
        i = 0
        while not self._io.is_eof():
            self.dataset.append(Dicom.DataElement(self._io, self, self._root))
            i += 1


    class DataElement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag_group = self._io.read_u2le()
            self.tag_element = self._io.read_u2le()
            self.vr = (self._io.read_bytes(2)).decode(u"ASCII")
            self.value_length = Dicom.LengthField(self._io, self, self._root)
            _on = self.vr
            if _on == u"TM":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"UI":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DA":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DS":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"ST":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"AS":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"CS":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"UT":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"IS":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DT":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"UN":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawValue(_io__raw_value, self, self._root)
            elif _on == u"PN":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"AE":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"LO":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"SQ":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.SequenceValue(_io__raw_value, self, self._root)
            elif _on == u"SH":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"LT":
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            else:
                self._raw_value = self._io.read_bytes(self.value_length.length)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawValue(_io__raw_value, self, self._root)


    class SequenceItem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.item_tag = self._io.read_bytes(4)
            if not self.item_tag == b"\xFE\xFF\x00\xE0":
                raise kaitaistruct.ValidationNotEqualError(b"\xFE\xFF\x00\xE0", self.item_tag, self._io, u"/types/sequence_item/seq/0")
            self.item_length = self._io.read_u4le()
            self.item_data = []
            for i in range(self.item_length):
                self.item_data.append(Dicom.DataElement(self._io, self, self._root))



    class SequenceValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.items = []
            i = 0
            while True:
                _ = Dicom.SequenceItem(self._io, self, self._root)
                self.items.append(_)
                if _.item_length == 0:
                    break
                i += 1


    class LengthField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self.is_explicit_vr:
                self.reserved = self._io.read_u2le()

            _on = self.is_explicit_vr
            if _on == True:
                self.length = self._io.read_u4le()
            elif _on == False:
                self.length = self._io.read_u2le()

        @property
        def is_explicit_vr(self):
            if hasattr(self, '_m_is_explicit_vr'):
                return self._m_is_explicit_vr

            self._m_is_explicit_vr =  ((self._parent.vr != u"OB") and (self._parent.vr != u"OW") and (self._parent.vr != u"OF") and (self._parent.vr != u"SQ") and (self._parent.vr != u"UN") and (self._parent.vr != u"UT")) 
            return getattr(self, '_m_is_explicit_vr', None)


    class RawValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = self._io.read_bytes_full()


    class StrValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"ASCII")



