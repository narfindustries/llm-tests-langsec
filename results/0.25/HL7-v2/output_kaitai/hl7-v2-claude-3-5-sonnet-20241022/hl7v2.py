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
            self.field_separator = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = Hl7v2.Hl7String(self._io, self, self._root)
            self.sending_facility = Hl7v2.Hl7String(self._io, self, self._root)
            self.receiving_application = Hl7v2.Hl7String(self._io, self, self._root)
            self.receiving_facility = Hl7v2.Hl7String(self._io, self, self._root)
            self.datetime = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.security = Hl7v2.Hl7String(self._io, self, self._root)
            self.message_type = Hl7v2.MessageType(self._io, self, self._root)
            self.message_control_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.processing_id = Hl7v2.ProcessingId(self._io, self, self._root)
            self.version_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.sequence_number = Hl7v2.Hl7Number(self._io, self, self._root)
            self.continuation_pointer = Hl7v2.Hl7String(self._io, self, self._root)
            self.accept_ack_type = Hl7v2.Hl7String(self._io, self, self._root)
            self.application_ack_type = Hl7v2.Hl7String(self._io, self, self._root)
            self.country_code = Hl7v2.Hl7String(self._io, self, self._root)


    class ProcessingId(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.processing_id = (self._io.read_bytes(1)).decode(u"ASCII")
            self.processing_mode = (self._io.read_bytes(1)).decode(u"ASCII")


    class Hl7String(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class UnknownSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"ASCII")


    class Hl7Number(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class MessageType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.message_code = Hl7v2.Hl7String(self._io, self, self._root)
            self.trigger_event = Hl7v2.Hl7String(self._io, self, self._root)
            self.message_structure = Hl7v2.Hl7String(self._io, self, self._root)


    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Hl7Number(self._io, self, self._root)
            self.value_type = Hl7v2.Hl7String(self._io, self, self._root)
            self.observation_identifier = Hl7v2.Hl7String(self._io, self, self._root)
            self.observation_sub_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.observation_value = Hl7v2.Hl7String(self._io, self, self._root)
            self.units = Hl7v2.Hl7String(self._io, self, self._root)
            self.reference_range = Hl7v2.Hl7String(self._io, self, self._root)
            self.abnormal_flags = Hl7v2.Hl7String(self._io, self, self._root)
            self.probability = Hl7v2.Hl7String(self._io, self, self._root)
            self.nature_of_abnormal_test = Hl7v2.Hl7String(self._io, self, self._root)
            self.observation_result_status = Hl7v2.Hl7String(self._io, self, self._root)
            self.effective_date_last_normal_value = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.user_defined_access_checks = Hl7v2.Hl7String(self._io, self, self._root)
            self.datetime_of_observation = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.producers_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.responsible_observer = Hl7v2.Hl7String(self._io, self, self._root)
            self.observation_method = Hl7v2.Hl7String(self._io, self, self._root)


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Hl7Number(self._io, self, self._root)
            self.patient_class = Hl7v2.Hl7String(self._io, self, self._root)
            self.assigned_patient_location = Hl7v2.Hl7String(self._io, self, self._root)
            self.admission_type = Hl7v2.Hl7String(self._io, self, self._root)
            self.preadmit_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.prior_patient_location = Hl7v2.Hl7String(self._io, self, self._root)
            self.attending_doctor = Hl7v2.Hl7String(self._io, self, self._root)
            self.referring_doctor = Hl7v2.Hl7String(self._io, self, self._root)
            self.consulting_doctor = Hl7v2.Hl7String(self._io, self, self._root)
            self.hospital_service = Hl7v2.Hl7String(self._io, self, self._root)
            self.temporary_location = Hl7v2.Hl7String(self._io, self, self._root)
            self.preadmit_test_indicator = Hl7v2.Hl7String(self._io, self, self._root)
            self.readmission_indicator = Hl7v2.Hl7String(self._io, self, self._root)
            self.admit_source = Hl7v2.Hl7String(self._io, self, self._root)
            self.ambulatory_status = Hl7v2.Hl7String(self._io, self, self._root)
            self.vip_indicator = Hl7v2.Hl7String(self._io, self, self._root)
            self.admitting_doctor = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_type = Hl7v2.Hl7String(self._io, self, self._root)
            self.visit_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.financial_class = Hl7v2.Hl7String(self._io, self, self._root)


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
            elif _on == u"PV1":
                self.content = Hl7v2.Pv1Segment(self._io, self, self._root)
            elif _on == u"OBR":
                self.content = Hl7v2.ObrSegment(self._io, self, self._root)
            elif _on == u"MSH":
                self.content = Hl7v2.MshSegment(self._io, self, self._root)
            else:
                self.content = Hl7v2.UnknownSegment(self._io, self, self._root)


    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Hl7Number(self._io, self, self._root)
            self.placer_order_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.filler_order_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.universal_service_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.priority = Hl7v2.Hl7String(self._io, self, self._root)
            self.requested_datetime = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.observation_datetime = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.observation_end_datetime = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.collection_volume = Hl7v2.Hl7String(self._io, self, self._root)
            self.collector_identifier = Hl7v2.Hl7String(self._io, self, self._root)
            self.specimen_action_code = Hl7v2.Hl7String(self._io, self, self._root)
            self.danger_code = Hl7v2.Hl7String(self._io, self, self._root)
            self.relevant_clinical_info = Hl7v2.Hl7String(self._io, self, self._root)
            self.specimen_received_datetime = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.specimen_source = Hl7v2.Hl7String(self._io, self, self._root)
            self.ordering_provider = Hl7v2.Hl7String(self._io, self, self._root)
            self.order_callback_phone_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.placers_field_1 = Hl7v2.Hl7String(self._io, self, self._root)
            self.placers_field_2 = Hl7v2.Hl7String(self._io, self, self._root)
            self.filler_field_1 = Hl7v2.Hl7String(self._io, self, self._root)
            self.filler_field_2 = Hl7v2.Hl7String(self._io, self, self._root)


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Hl7Number(self._io, self, self._root)
            self.patient_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_identifier_list = Hl7v2.Hl7String(self._io, self, self._root)
            self.alternate_patient_id = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_name = Hl7v2.Hl7String(self._io, self, self._root)
            self.mothers_maiden_name = Hl7v2.Hl7String(self._io, self, self._root)
            self.date_of_birth = Hl7v2.Hl7Datetime(self._io, self, self._root)
            self.sex = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_alias = Hl7v2.Hl7String(self._io, self, self._root)
            self.race = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_address = Hl7v2.Hl7String(self._io, self, self._root)
            self.county_code = Hl7v2.Hl7String(self._io, self, self._root)
            self.phone_number_home = Hl7v2.Hl7String(self._io, self, self._root)
            self.phone_number_business = Hl7v2.Hl7String(self._io, self, self._root)
            self.primary_language = Hl7v2.Hl7String(self._io, self, self._root)
            self.marital_status = Hl7v2.Hl7String(self._io, self, self._root)
            self.religion = Hl7v2.Hl7String(self._io, self, self._root)
            self.patient_account_number = Hl7v2.Hl7String(self._io, self, self._root)
            self.ssn = Hl7v2.Hl7String(self._io, self, self._root)
            self.drivers_license = Hl7v2.Hl7String(self._io, self, self._root)


    class Hl7Datetime(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")



