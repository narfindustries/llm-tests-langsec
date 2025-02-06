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
        self.data_elements = []
        i = 0
        while not self._io.is_eof():
            self.data_elements.append(Dicom.DataElement(self._io, self, self._root))
            i += 1


    class PatientBirthDate(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class PatientId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class PixelRepresentation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


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

            if self.is_explicit_vr_long:
                self.reserved = self._io.read_u2le()

            if self.is_implicit_vr:
                self.length = self._io.read_u4le()

            if not (self.is_implicit_vr):
                self.length_explicit = Dicom.LengthField(self._io, self, self._root)

            _on = self.tag
            if _on == 2621442:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.SamplesPerPixel(_io__raw_data, self, self._root)
            elif _on == 2097169:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.SeriesNumber(_io__raw_data, self, self._root)
            elif _on == 524384:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.Modality(_io__raw_data, self, self._root)
            elif _on == 2621696:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.BitsAllocated(_io__raw_data, self, self._root)
            elif _on == 1048640:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.PatientSex(_io__raw_data, self, self._root)
            elif _on == 2621697:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.BitsStored(_io__raw_data, self, self._root)
            elif _on == 1048624:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.PatientBirthDate(_io__raw_data, self, self._root)
            elif _on == 2097165:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.StudyInstanceUid(_io__raw_data, self, self._root)
            elif _on == 2097166:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.SeriesInstanceUid(_io__raw_data, self, self._root)
            elif _on == 131088:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.TransferSyntax(_io__raw_data, self, self._root)
            elif _on == 2097168:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.StudyId(_io__raw_data, self, self._root)
            elif _on == 2621457:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.Columns(_io__raw_data, self, self._root)
            elif _on == 2621698:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.HighBit(_io__raw_data, self, self._root)
            elif _on == 1048608:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.PatientId(_io__raw_data, self, self._root)
            elif _on == 1048592:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.PatientName(_io__raw_data, self, self._root)
            elif _on == 2621699:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.PixelRepresentation(_io__raw_data, self, self._root)
            elif _on == 2621456:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.Rows(_io__raw_data, self, self._root)
            else:
                self._raw_data = self._io.read_bytes(self.data_size)
                _io__raw_data = KaitaiStream(BytesIO(self._raw_data))
                self.data = Dicom.RawData(_io__raw_data, self, self._root)

        @property
        def tag(self):
            if hasattr(self, '_m_tag'):
                return self._m_tag

            self._m_tag = ((self.tag_group << 16) | self.tag_element)
            return getattr(self, '_m_tag', None)

        @property
        def data_size(self):
            if hasattr(self, '_m_data_size'):
                return self._m_data_size

            if not (self.is_implicit_vr):
                self._m_data_size = self.length_explicit.value

            return getattr(self, '_m_data_size', None)

        @property
        def is_explicit_vr_long(self):
            if hasattr(self, '_m_is_explicit_vr_long'):
                return self._m_is_explicit_vr_long

            self._m_is_explicit_vr_long =  ((self.vr == u"OB") or (self.vr == u"OW") or (self.vr == u"OF") or (self.vr == u"SQ") or (self.vr == u"UT") or (self.vr == u"UN")) 
            return getattr(self, '_m_is_explicit_vr_long', None)

        @property
        def data_size_implicit(self):
            if hasattr(self, '_m_data_size_implicit'):
                return self._m_data_size_implicit

            if self.is_implicit_vr:
                self._m_data_size_implicit = self.length

            return getattr(self, '_m_data_size_implicit', None)

        @property
        def is_implicit_vr(self):
            if hasattr(self, '_m_is_implicit_vr'):
                return self._m_is_implicit_vr

            self._m_is_implicit_vr = self._root.transfer_syntax_implicit
            return getattr(self, '_m_is_implicit_vr', None)


    class StudyId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class BitsStored(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    class StudyInstanceUid(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class RawData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_bytes(self._parent.data_size)


    class HighBit(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    class SamplesPerPixel(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    class PatientSex(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class SeriesNumber(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class LengthField(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            _on = self._parent.is_explicit_vr_long
            if _on == True:
                self.value = self._io.read_u4le()
            elif _on == False:
                self.value = self._io.read_u2le()


    class PatientName(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class Columns(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    class Modality(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class SeriesInstanceUid(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(self._parent.data_size)).decode(u"ASCII")


    class TransferSyntax(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class BitsAllocated(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    class Rows(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u2le()


    @property
    def transfer_syntax_implicit(self):
        if hasattr(self, '_m_transfer_syntax_implicit'):
            return self._m_transfer_syntax_implicit

        self._m_transfer_syntax_implicit = True
        return getattr(self, '_m_transfer_syntax_implicit', None)


