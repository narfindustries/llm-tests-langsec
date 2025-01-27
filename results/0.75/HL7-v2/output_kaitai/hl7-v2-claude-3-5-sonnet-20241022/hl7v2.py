# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7v2(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Hl7v2.Segment(self._io, self, self._root))
            i += 1


    class MshSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = self._io.read_u1()
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.sending_facility = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.receiving_application = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.receiving_facility = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.security = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.message_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.message_control_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.processing_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.version_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.sequence_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.continuation_pointer = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.accept_ack_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.application_ack_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.country_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.character_set = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.principal_language = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.components = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class EvnSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.event_type_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.recorded_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.planned_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.event_reason_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.operator_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.event_occurred = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.value_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_identifier = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_sub_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.units = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.references_range = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.abnormal_flags = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.probability = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.nature_of_abnormal_test = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_result_status = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.effective_date_of_reference_range = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.user_defined_access_checks = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.datetime_of_the_observation = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.producers_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.responsible_observer = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_method = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_class = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.assigned_patient_location = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.admission_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.preadmit_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.prior_patient_location = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.attending_doctor = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.referring_doctor = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.consulting_doctor = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.hospital_service = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.temporary_location = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.preadmit_test_indicator = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.readmission_indicator = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.admit_source = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.ambulatory_status = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.vip_indicator = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.admitting_doctor = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.visit_number = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


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
                self.content = Hl7v2.PidSegment(self._io, self, self._root)
            elif _on == u"OBX":
                self.content = Hl7v2.ObxSegment(self._io, self, self._root)
            elif _on == u"EVN":
                self.content = Hl7v2.EvnSegment(self._io, self, self._root)
            elif _on == u"PV1":
                self.content = Hl7v2.Pv1Segment(self._io, self, self._root)
            elif _on == u"OBR":
                self.content = Hl7v2.ObrSegment(self._io, self, self._root)
            elif _on == u"MSH":
                self.content = Hl7v2.MshSegment(self._io, self, self._root)
            else:
                self.content = Hl7v2.DefaultSegment(self._io, self, self._root)


    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.placer_order_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.filler_order_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.universal_service_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.priority = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.requested_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.observation_end_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.collection_volume = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.collector_identifier = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.specimen_action_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.danger_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.relevant_clinical_info = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.specimen_received_datetime = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.specimen_source = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.ordering_provider = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.order_callback_phone_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.placers_field_1 = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.placers_field_2 = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_identifier_list = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.alternate_patient_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_name = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.mothers_maiden_name = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.datetime_of_birth = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.administrative_sex = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_alias = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.race = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_address = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.county_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.phone_number_home = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.phone_number_business = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.primary_language = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.marital_status = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.religion = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_account_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.ssn_number = (self._io.read_bytes_term(13, False, True, True)).decode(u"ASCII")


    class DefaultSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.fields = []
            i = 0
            while not self._io.is_eof():
                self.fields.append(Hl7v2.Field(self._io, self, self._root))
                i += 1




