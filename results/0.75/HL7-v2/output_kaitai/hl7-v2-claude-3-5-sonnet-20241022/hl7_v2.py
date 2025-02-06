# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
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


    class MshSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = Hl7V2.Hl7String(self._io, self, self._root)
            self.sending_facility = Hl7V2.Hl7String(self._io, self, self._root)
            self.receiving_application = Hl7V2.Hl7String(self._io, self, self._root)
            self.receiving_facility = Hl7V2.Hl7String(self._io, self, self._root)
            self.datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.security = Hl7V2.Hl7String(self._io, self, self._root)
            self.message_type = Hl7V2.Hl7String(self._io, self, self._root)
            self.message_control_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.processing_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.version_id = Hl7V2.Hl7String(self._io, self, self._root)


    class Hl7String(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_full()).decode(u"ASCII")


    class EvnSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.event_type_code = Hl7V2.Hl7String(self._io, self, self._root)
            self.recorded_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.planned_event_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.event_reason_code = Hl7V2.Hl7String(self._io, self, self._root)
            self.operator_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.event_occurred = Hl7V2.Hl7Datetime(self._io, self, self._root)


    class Nk1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.name = Hl7V2.Hl7String(self._io, self, self._root)
            self.relationship = Hl7V2.Hl7String(self._io, self, self._root)
            self.address = Hl7V2.Hl7String(self._io, self, self._root)
            self.phone_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.business_phone_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.contact_role = Hl7V2.Hl7String(self._io, self, self._root)
            self.start_date = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.end_date = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.next_of_kin_job_title = Hl7V2.Hl7String(self._io, self, self._root)
            self.next_of_kin_job_code = Hl7V2.Hl7String(self._io, self, self._root)
            self.next_of_kin_employee_number = Hl7V2.Hl7String(self._io, self, self._root)


    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.value_type = Hl7V2.Hl7String(self._io, self, self._root)
            self.observation_identifier = Hl7V2.Hl7String(self._io, self, self._root)
            self.observation_sub_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.observation_value = Hl7V2.Hl7String(self._io, self, self._root)
            self.units = Hl7V2.Hl7String(self._io, self, self._root)
            self.references_range = Hl7V2.Hl7String(self._io, self, self._root)
            self.abnormal_flags = Hl7V2.Hl7String(self._io, self, self._root)
            self.probability = Hl7V2.Hl7String(self._io, self, self._root)
            self.nature_of_abnormal_test = Hl7V2.Hl7String(self._io, self, self._root)
            self.observation_result_status = Hl7V2.Hl7String(self._io, self, self._root)
            self.effective_date = Hl7V2.Hl7Datetime(self._io, self, self._root)


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_class = Hl7V2.Hl7String(self._io, self, self._root)
            self.assigned_patient_location = Hl7V2.Hl7String(self._io, self, self._root)
            self.admission_type = Hl7V2.Hl7String(self._io, self, self._root)
            self.preadmit_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.prior_patient_location = Hl7V2.Hl7String(self._io, self, self._root)
            self.attending_doctor = Hl7V2.Hl7String(self._io, self, self._root)
            self.referring_doctor = Hl7V2.Hl7String(self._io, self, self._root)
            self.consulting_doctor = Hl7V2.Hl7String(self._io, self, self._root)
            self.hospital_service = Hl7V2.Hl7String(self._io, self, self._root)
            self.temporary_location = Hl7V2.Hl7String(self._io, self, self._root)
            self.preadmit_test_indicator = Hl7V2.Hl7String(self._io, self, self._root)
            self.admission_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.discharge_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_type = (self._io.read_bytes(3)).decode(u"ASCII")
            _on = self.segment_type
            if _on == u"PID":
                self.fields = Hl7V2.PidSegment(self._io, self, self._root)
            elif _on == u"OBX":
                self.fields = Hl7V2.ObxSegment(self._io, self, self._root)
            elif _on == u"EVN":
                self.fields = Hl7V2.EvnSegment(self._io, self, self._root)
            elif _on == u"PV1":
                self.fields = Hl7V2.Pv1Segment(self._io, self, self._root)
            elif _on == u"OBR":
                self.fields = Hl7V2.ObrSegment(self._io, self, self._root)
            elif _on == u"MSH":
                self.fields = Hl7V2.MshSegment(self._io, self, self._root)
            elif _on == u"NK1":
                self.fields = Hl7V2.Nk1Segment(self._io, self, self._root)


    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.placer_order_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.filler_order_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.universal_service_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.priority = Hl7V2.Hl7String(self._io, self, self._root)
            self.requested_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.observation_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.observation_end_datetime = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.collection_volume = Hl7V2.Hl7String(self._io, self, self._root)
            self.collector_identifier = Hl7V2.Hl7String(self._io, self, self._root)
            self.specimen_action_code = Hl7V2.Hl7String(self._io, self, self._root)
            self.danger_code = Hl7V2.Hl7String(self._io, self, self._root)


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_identifier_list = Hl7V2.Hl7String(self._io, self, self._root)
            self.alternate_patient_id = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_name = Hl7V2.Hl7String(self._io, self, self._root)
            self.mothers_maiden_name = Hl7V2.Hl7String(self._io, self, self._root)
            self.datetime_of_birth = Hl7V2.Hl7Datetime(self._io, self, self._root)
            self.administrative_sex = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_alias = Hl7V2.Hl7String(self._io, self, self._root)
            self.race = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_address = Hl7V2.Hl7String(self._io, self, self._root)
            self.county_code = Hl7V2.Hl7String(self._io, self, self._root)
            self.phone_number_home = Hl7V2.Hl7String(self._io, self, self._root)
            self.phone_number_business = Hl7V2.Hl7String(self._io, self, self._root)
            self.primary_language = Hl7V2.Hl7String(self._io, self, self._root)
            self.marital_status = Hl7V2.Hl7String(self._io, self, self._root)
            self.religion = Hl7V2.Hl7String(self._io, self, self._root)
            self.patient_account_number = Hl7V2.Hl7String(self._io, self, self._root)
            self.ssn_number = Hl7V2.Hl7String(self._io, self, self._root)


    class Hl7Datetime(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes(14)).decode(u"ASCII")



