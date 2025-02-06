# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):

    class MessageType(Enum):
        adt = 1
        orm = 2
        oru = 3
        mdm = 4

    class PatientClass(Enum):
        inpatient = 1
        outpatient = 2
        emergency = 3

    class Sex(Enum):
        male = 1
        female = 2
        other = 3
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.message = Hl7V2.Hl7Message(self._io, self, self._root)

    class MshSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ascii")
            self.sending_application = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.sending_facility = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.receiving_application = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.receiving_facility = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.message_datetime = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.security = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            self.message_type = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.message_control_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.processing_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.version_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ascii")
            self.repetitions = []
            for i in range(0):
                self.repetitions.append(Hl7V2.Field(self._io, self, self._root))



    class Hl7Message(KaitaiStruct):
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



    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.value_type = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.observation_identifier = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.observation_value = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.units = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.reference_range = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.abnormal_flags = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")



    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_class = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.assigned_patient_location = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.admission_type = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.preadmit_number = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.prior_patient_location = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.attending_doctor = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.referring_doctor = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.consulting_doctor = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")



    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_name = (self._io.read_bytes(3)).decode(u"ascii")
            self.fields = []
            i = 0
            while not self._io.is_eof():
                self.fields.append(Hl7V2.Field(self._io, self, self._root))
                i += 1



    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.placer_order_number = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.filler_order_number = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.universal_service_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.priority = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.requested_datetime = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.observation_datetime = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.observation_end_datetime = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")



    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.patient_id = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.patient_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.mother_maiden_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            self.date_of_birth = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            self.sex = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.patient_alias = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            if self._io.pos() < self._io.size():
                self.race = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")

            self.patient_address = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")
            if self._io.pos() < self._io.size():
                self.country_code = (self._io.read_bytes_term(0, False, True, True)).decode(u"ascii")




