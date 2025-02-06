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
        self.file_preamble = self._io.read_bytes(128)
        self.dicom_prefix = self._io.read_bytes(4)
        if not self.dicom_prefix == b"\x44\x49\x43\x4D":
            raise kaitaistruct.ValidationNotEqualError(b"\x44\x49\x43\x4D", self.dicom_prefix, self._io, u"/seq/1")
        self.file_meta_information_group = Dicom.FileMetaInfo(self._io, self, self._root)
        self.dataset = Dicom.DatasetSequence(self._io, self, self._root)

    class PatientInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_name = Dicom.TagElement(self._io, self, self._root)
            self.patient_id = Dicom.TagElement(self._io, self, self._root)
            self.patient_birth_date = Dicom.TagElement(self._io, self, self._root)
            self.patient_sex = Dicom.TagElement(self._io, self, self._root)
            if self._parent._io.size() > self._io.pos():
                self.patient_age = Dicom.TagElement(self._io, self, self._root)



    class StudyInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.study_instance_uid = Dicom.TagElement(self._io, self, self._root)
            self.study_date = Dicom.TagElement(self._io, self, self._root)
            self.study_time = Dicom.TagElement(self._io, self, self._root)
            self.accession_number = Dicom.TagElement(self._io, self, self._root)
            if self._parent._io.size() > self._io.pos():
                self.referring_physician = Dicom.TagElement(self._io, self, self._root)



    class DatasetSequence(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_tags = Dicom.PatientInformation(self._io, self, self._root)
            self.study_tags = Dicom.StudyInformation(self._io, self, self._root)
            self.series_tags = Dicom.SeriesInformation(self._io, self, self._root)
            self.image_tags = Dicom.ImageInformation(self._io, self, self._root)


    class SeriesInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.series_instance_uid = Dicom.TagElement(self._io, self, self._root)
            self.modality = Dicom.TagElement(self._io, self, self._root)
            self.series_number = Dicom.TagElement(self._io, self, self._root)
            if self._parent._io.size() > self._io.pos():
                self.series_description = Dicom.TagElement(self._io, self, self._root)



    class TagElement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group = self._io.read_u2le()
            self.element = self._io.read_u2le()
            self.len_value = self._io.read_u4le()
            self.value = self._io.read_bytes(self.len_value)


    class PixelSequence(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixel_representation = Dicom.TagElement(self._io, self, self._root)
            self.rows = Dicom.TagElement(self._io, self, self._root)
            self.columns = Dicom.TagElement(self._io, self, self._root)
            self.bits_allocated = Dicom.TagElement(self._io, self, self._root)
            self.bits_stored = Dicom.TagElement(self._io, self, self._root)
            self.pixel_data_element = Dicom.TagElement(self._io, self, self._root)


    class ImageInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sop_class_uid = Dicom.TagElement(self._io, self, self._root)
            self.sop_instance_uid = Dicom.TagElement(self._io, self, self._root)
            self.image_type = Dicom.TagElement(self._io, self, self._root)
            self.pixel_data = Dicom.PixelSequence(self._io, self, self._root)


    class FileMetaInfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.group_length = Dicom.TagElement(self._io, self, self._root)
            self.transfer_syntax = Dicom.TagElement(self._io, self, self._root)
            self.sop_class_uid = Dicom.TagElement(self._io, self, self._root)
            self.sop_instance_uid = Dicom.TagElement(self._io, self, self._root)
            self.implementation_class_uid = Dicom.TagElement(self._io, self, self._root)



