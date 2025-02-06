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
        self.file_meta_information_group = Dicom.FileMetaInformation(self._io, self, self._root)
        self.transfer_syntax_uid = self._io.read_bytes(64)
        self.implementation_class_uid = self._io.read_bytes(64)
        self.content = Dicom.DicomDataSet(self._io, self, self._root)

    class FileMetaInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_meta_information_version = self._io.read_bytes(2)
            self.media_storage_sop_instance_uid = self._io.read_bytes(64)
            self.media_storage_sop_class_uid = self._io.read_bytes(64)
            self.transfer_syntax_uid = self._io.read_bytes(64)
            self.implementation_class_uid = self._io.read_bytes(64)
            self.source_application_entity_title = self._io.read_bytes(64)
            self.sending_application_entity_title = self._io.read_bytes(64)
            self.receiving_application_entity_title = self._io.read_bytes(64)
            self.private_implementation_data = self._io.read_bytes(64)


    class PatientInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_name = self._io.read_bytes(64)
            self.patient_id = self._io.read_bytes(64)
            self.patient_birth_date = self._io.read_bytes(8)
            self.patient_sex = self._io.read_bytes(1)
            self.patient_age = self._io.read_bytes(2)
            self.patient_weight = self._io.read_bytes(4)
            self.patient_address = self._io.read_bytes(64)
            self.patient_phone = self._io.read_bytes(64)


    class StudyInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.study_instance_uid = self._io.read_bytes(64)
            self.study_date = self._io.read_bytes(8)
            self.study_time = self._io.read_bytes(6)
            self.study_id = self._io.read_bytes(64)
            self.study_description = self._io.read_bytes(64)
            self.study_status = self._io.read_bytes(1)


    class SeriesInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.series_instance_uid = self._io.read_bytes(64)
            self.series_date = self._io.read_bytes(8)
            self.series_time = self._io.read_bytes(6)
            self.series_number = self._io.read_bytes(2)
            self.series_description = self._io.read_bytes(64)
            self.series_status = self._io.read_bytes(1)


    class OverlayData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.overlay_data = self._io.read_bytes((self._parent._io.size() - self._parent._io.pos()))


    class CurveData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.curve_data = self._io.read_bytes((self._parent._io.size() - self._parent._io.pos()))


    class WaveData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.wave_data = self._io.read_bytes((self._parent._io.size() - self._parent._io.pos()))


    class PrivateData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.private_data = self._io.read_bytes((self._parent._io.size() - self._parent._io.pos()))


    class ImageInformation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sop_instance_uid = self._io.read_bytes(64)
            self.image_type = self._io.read_bytes(64)
            self.image_date = self._io.read_bytes(8)
            self.image_time = self._io.read_bytes(6)
            self.image_number = self._io.read_bytes(2)
            self.image_description = self._io.read_bytes(64)
            self.patient_position = self._io.read_bytes(1)
            self.image_orientation = self._io.read_bytes(6)


    class PixelData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixel_data = self._io.read_bytes((self._parent._io.size() - self._parent._io.pos()))


    class DicomDataSet(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_information = Dicom.PatientInformation(self._io, self, self._root)
            self.study_information = Dicom.StudyInformation(self._io, self, self._root)
            self.series_information = Dicom.SeriesInformation(self._io, self, self._root)
            self.image_information = Dicom.ImageInformation(self._io, self, self._root)
            self.pixel_data = Dicom.PixelData(self._io, self, self._root)
            self.overlay_data = Dicom.OverlayData(self._io, self, self._root)
            self.curve_data = Dicom.CurveData(self._io, self, self._root)
            self.wave_data = Dicom.WaveData(self._io, self, self._root)
            self.private_data = Dicom.PrivateData(self._io, self, self._root)



