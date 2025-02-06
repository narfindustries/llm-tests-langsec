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
        self.dicom_prefix = (self._io.read_bytes(4)).decode(u"ASCII")
        self.meta_header = Dicom.MetaInformation(self._io, self, self._root)
        self.dataset = Dicom.DicomDataset(self._io, self, self._root)

    class StudyModule(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.study_instance_uid = Dicom.Attribute(self._io, self, self._root)
            self.study_date = Dicom.Attribute(self._io, self, self._root)
            self.study_time = Dicom.Attribute(self._io, self, self._root)
            self.accession_number = Dicom.Attribute(self._io, self, self._root)
            self.referring_physician_name = Dicom.Attribute(self._io, self, self._root)
            self.study_description = Dicom.Attribute(self._io, self, self._root)


    class SeriesModule(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.modality = Dicom.Attribute(self._io, self, self._root)
            self.series_instance_uid = Dicom.Attribute(self._io, self, self._root)
            self.series_number = Dicom.Attribute(self._io, self, self._root)
            self.series_description = Dicom.Attribute(self._io, self, self._root)
            self.body_part_examined = Dicom.Attribute(self._io, self, self._root)


    class DicomDataset(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_module = Dicom.PatientModule(self._io, self, self._root)
            self.study_module = Dicom.StudyModule(self._io, self, self._root)
            self.series_module = Dicom.SeriesModule(self._io, self, self._root)
            self.image_module = Dicom.ImageModule(self._io, self, self._root)


    class PixelData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4le()
            self.compressed_data = self._io.read_bytes(self.length)


    class Attribute(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.tag = self._io.read_u4le()
            self.vr = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.length = self._io.read_u2le()
            self.value = (self._io.read_bytes(self.length)).decode(u"UTF-8")


    class PatientModule(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_name = Dicom.Attribute(self._io, self, self._root)
            self.patient_id = Dicom.Attribute(self._io, self, self._root)
            self.patient_birth_date = Dicom.Attribute(self._io, self, self._root)
            self.patient_sex = Dicom.Attribute(self._io, self, self._root)
            self.patient_age = Dicom.Attribute(self._io, self, self._root)


    class ImageModule(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sop_instance_uid = Dicom.Attribute(self._io, self, self._root)
            self.image_number = Dicom.Attribute(self._io, self, self._root)
            self.pixel_data = Dicom.PixelData(self._io, self, self._root)
            self.rows = Dicom.Attribute(self._io, self, self._root)
            self.columns = Dicom.Attribute(self._io, self, self._root)
            self.bits_allocated = Dicom.Attribute(self._io, self, self._root)
            self.photometric_interpretation = Dicom.Attribute(self._io, self, self._root)


    class MetaInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_meta_information_group_length = Dicom.Attribute(self._io, self, self._root)
            self.file_meta_information_version = Dicom.Attribute(self._io, self, self._root)
            self.media_storage_sop_class_uid = Dicom.Attribute(self._io, self, self._root)
            self.media_storage_sop_instance_uid = Dicom.Attribute(self._io, self, self._root)
            self.transfer_syntax_uid = Dicom.Attribute(self._io, self, self._root)
            self.implementation_class_uid = Dicom.Attribute(self._io, self, self._root)
            self.implementation_version_name = Dicom.Attribute(self._io, self, self._root)



