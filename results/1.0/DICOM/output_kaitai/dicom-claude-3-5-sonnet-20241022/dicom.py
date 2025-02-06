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
        self.dicom_prefix = self._io.read_bytes(4)
        if not self.dicom_prefix == b"\x44\x49\x43\x4D":
            raise kaitaistruct.ValidationNotEqualError(b"\x44\x49\x43\x4D", self.dicom_prefix, self._io, u"/seq/1")
        self.meta_information = Dicom.MetaInformationGroup(self._io, self, self._root)
        self.elements = []
        i = 0
        while not self._io.is_eof():
            self.elements.append(Dicom.DataElement(self._io, self, self._root))
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
            if not (self.is_implicit_vr):
                self.vr = (self._io.read_bytes(2)).decode(u"ASCII")

            if not (self.is_implicit_vr):
                _on = self.vr
                if _on == u"UT":
                    self.length = self._io.read_u4le()
                elif _on == u"OW":
                    self.length = self._io.read_u4le()
                elif _on == u"OF":
                    self.length = self._io.read_u4le()
                elif _on == u"OB":
                    self.length = self._io.read_u4le()
                elif _on == u"UN":
                    self.length = self._io.read_u4le()
                elif _on == u"SQ":
                    self.length = self._io.read_u4le()
                else:
                    self.length = self._io.read_u2le()

            if self.is_implicit_vr:
                self.length_implicit = self._io.read_u4le()

            _on = self.vr
            if _on == u"TM":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"UI":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DA":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DS":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"FL":
                self.value = self._io.read_f4le()
            elif _on == u"UL":
                self.value = self._io.read_u4le()
            elif _on == u"ST":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"AS":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"CS":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"UT":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"SS":
                self.value = self._io.read_s2le()
            elif _on == u"OW":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)
            elif _on == u"OF":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)
            elif _on == u"IS":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"DT":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"OB":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)
            elif _on == u"UN":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)
            elif _on == u"OD":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)
            elif _on == u"PN":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"FD":
                self.value = self._io.read_f8le()
            elif _on == u"SL":
                self.value = self._io.read_s4le()
            elif _on == u"US":
                self.value = self._io.read_u2le()
            elif _on == u"AE":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"LO":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"SQ":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.Sequence(_io__raw_value, self, self._root)
            elif _on == u"SH":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"LT":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.StrValue(_io__raw_value, self, self._root)
            elif _on == u"AT":
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.TagValue(_io__raw_value, self, self._root)
            else:
                self._raw_value = self._io.read_bytes(self.length_value)
                _io__raw_value = KaitaiStream(BytesIO(self._raw_value))
                self.value = Dicom.RawBytes(_io__raw_value, self, self._root)

        @property
        def tag(self):
            if hasattr(self, '_m_tag'):
                return self._m_tag

            self._m_tag = ((self.tag_group << 16) | self.tag_element)
            return getattr(self, '_m_tag', None)

        @property
        def is_implicit_vr(self):
            if hasattr(self, '_m_is_implicit_vr'):
                return self._m_is_implicit_vr

            self._m_is_implicit_vr = self._root.meta_information.transfer_syntax.value.value == u"1.2.840.10008.1.2"
            return getattr(self, '_m_is_implicit_vr', None)

        @property
        def length_value(self):
            if hasattr(self, '_m_length_value'):
                return self._m_length_value

            self._m_length_value = (self.length_implicit if self.is_implicit_vr else self.length)
            return getattr(self, '_m_length_value', None)


    class TagValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2le()
            self.element = self._io.read_u2le()


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
            self.elements = []
            for i in range(self.item_length // 8):
                self.elements.append(Dicom.DataElement(self._io, self, self._root))



    class MetaInformationGroup(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transfer_syntax = Dicom.DataElement(self._io, self, self._root)


    class Sequence(KaitaiStruct):
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


    class RawBytes(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bytes_full()


    class StrValue(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"ASCII")



