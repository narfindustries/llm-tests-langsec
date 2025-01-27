# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
    """Health Level Seven (HL7) is a set of international standards for transfer of clinical and administrative data between software applications used by various healthcare providers.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Hl7V2.Segment(self._io, self, self._root))
            i += 1


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_type = (self._io.read_bytes(3)).decode(u"ASCII")

        @property
        def message_header(self):
            if hasattr(self, '_m_message_header'):
                return self._m_message_header

            if self.segment_type == u"MSH":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_message_header = Hl7V2.Msh(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_message_header', None)

        @property
        def patient_identification(self):
            if hasattr(self, '_m_patient_identification'):
                return self._m_patient_identification

            if self.segment_type == u"PID":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_patient_identification = Hl7V2.Pid(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_patient_identification', None)

        @property
        def patient_visit(self):
            if hasattr(self, '_m_patient_visit'):
                return self._m_patient_visit

            if self.segment_type == u"PV1":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_patient_visit = Hl7V2.Pv1(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_patient_visit', None)


    class Msh(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = (KaitaiStream.bytes_terminate(self._io.read_bytes(180), 124, False)).decode(u"ASCII")
            self.sending_facility = (KaitaiStream.bytes_terminate(self._io.read_bytes(180), 124, False)).decode(u"ASCII")
            self.receiving_application = (KaitaiStream.bytes_terminate(self._io.read_bytes(180), 124, False)).decode(u"ASCII")
            self.receiving_facility = (KaitaiStream.bytes_terminate(self._io.read_bytes(180), 124, False)).decode(u"ASCII")
            self.date_time_of_message = (KaitaiStream.bytes_terminate(self._io.read_bytes(26), 124, False)).decode(u"ASCII")
            self.security = (KaitaiStream.bytes_terminate(self._io.read_bytes(40), 124, False)).decode(u"ASCII")
            self.message_type = (KaitaiStream.bytes_terminate(self._io.read_bytes(7), 124, False)).decode(u"ASCII")
            self.message_control_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(199), 124, False)).decode(u"ASCII")
            self.processing_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(3), 124, False)).decode(u"ASCII")
            self.version_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(60), 124, False)).decode(u"ASCII")


    class Pid(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(4), 124, False)).decode(u"ASCII")
            self.patient_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(20), 124, False)).decode(u"ASCII")
            self.patient_identifier_list = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.alternate_patient_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(20), 124, False)).decode(u"ASCII")
            self.patient_name = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.mother_maiden_name = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.date_of_birth = (KaitaiStream.bytes_terminate(self._io.read_bytes(8), 124, False)).decode(u"ASCII")
            self.sex = (KaitaiStream.bytes_terminate(self._io.read_bytes(1), 124, False)).decode(u"ASCII")
            self.patient_address = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.phone_number_home = (KaitaiStream.bytes_terminate(self._io.read_bytes(40), 124, False)).decode(u"ASCII")
            self.phone_number_business = (KaitaiStream.bytes_terminate(self._io.read_bytes(40), 124, False)).decode(u"ASCII")


    class Pv1(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(4), 124, False)).decode(u"ASCII")
            self.patient_class = (KaitaiStream.bytes_terminate(self._io.read_bytes(1), 124, False)).decode(u"ASCII")
            self.assigned_patient_location = (KaitaiStream.bytes_terminate(self._io.read_bytes(80), 124, False)).decode(u"ASCII")
            self.admission_type = (KaitaiStream.bytes_terminate(self._io.read_bytes(2), 124, False)).decode(u"ASCII")
            self.preadmit_number = (KaitaiStream.bytes_terminate(self._io.read_bytes(20), 124, False)).decode(u"ASCII")
            self.prior_patient_location = (KaitaiStream.bytes_terminate(self._io.read_bytes(80), 124, False)).decode(u"ASCII")
            self.attending_doctor = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.referring_doctor = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.consulting_doctor = (KaitaiStream.bytes_terminate(self._io.read_bytes(250), 124, False)).decode(u"ASCII")
            self.hospital_service = (KaitaiStream.bytes_terminate(self._io.read_bytes(3), 124, False)).decode(u"ASCII")
            self.admission_date = (KaitaiStream.bytes_terminate(self._io.read_bytes(26), 124, False)).decode(u"ASCII")
            self.discharge_date = (KaitaiStream.bytes_terminate(self._io.read_bytes(26), 124, False)).decode(u"ASCII")



