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
        self.file_meta_information_group_length = self._io.read_u4le()
        self.file_meta_information_version = self._io.read_u2le()
        self.media_storage_sop_class_uid = Dicom.DicomUi(self._io, self, self._root)
        self.media_storage_sop_instance_uid = Dicom.DicomUi(self._io, self, self._root)
        self.transfer_syntax_uid = Dicom.DicomUi(self._io, self, self._root)
        self.implementation_class_uid = Dicom.DicomUi(self._io, self, self._root)
        self.implementation_version_name = Dicom.DicomLo(self._io, self, self._root)
        self.source_application_entity_title = Dicom.DicomAe(self._io, self, self._root)
        self.sending_application_entity_title = Dicom.DicomAe(self._io, self, self._root)
        self.receiving_application_entity_title = Dicom.DicomAe(self._io, self, self._root)
        self.private_information_creator_uid = Dicom.DicomUi(self._io, self, self._root)
        self.private_information = Dicom.DicomOb(self._io, self, self._root)
        self.elements = []
        i = 0
        while not self._io.is_eof():
            self.elements.append(Dicom.Element(self._io, self, self._root))
            i += 1


    class DicomAe(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"ASCII")


    class Tag(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2le()
            self.element = self._io.read_u2le()


    class DicomUi(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"ASCII")


    class DicomVr(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(2)).decode(u"ASCII")


    class Element(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = Dicom.Tag(self._io, self, self._root)
            self.vr = Dicom.DicomVr(self._io, self, self._root)
            self.length = self._io.read_u2le()
            self.value = self._io.read_bytes(self.length)


    class DicomOb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = []
            i = 0
            while not self._io.is_eof():
                self.value.append(self._io.read_u1())
                i += 1



    class DicomLo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"ASCII")



