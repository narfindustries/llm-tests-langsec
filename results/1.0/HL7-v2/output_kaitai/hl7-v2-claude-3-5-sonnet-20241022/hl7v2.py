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
            self.sending_application = Hl7v2.Field(self._io, self, self._root)
            self.sending_facility = Hl7v2.Field(self._io, self, self._root)
            self.receiving_application = Hl7v2.Field(self._io, self, self._root)
            self.receiving_facility = Hl7v2.Field(self._io, self, self._root)
            self.datetime = Hl7v2.Field(self._io, self, self._root)
            self.security = Hl7v2.Field(self._io, self, self._root)
            self.message_type = Hl7v2.Field(self._io, self, self._root)
            self.message_control_id = Hl7v2.Field(self._io, self, self._root)
            self.processing_id = Hl7v2.Field(self._io, self, self._root)
            self.version_id = Hl7v2.Field(self._io, self, self._root)
            self.sequence_number = Hl7v2.Field(self._io, self, self._root)
            self.continuation_pointer = Hl7v2.Field(self._io, self, self._root)
            self.accept_ack_type = Hl7v2.Field(self._io, self, self._root)
            self.application_ack_type = Hl7v2.Field(self._io, self, self._root)
            self.country_code = Hl7v2.Field(self._io, self, self._root)
            self.character_set = Hl7v2.Field(self._io, self, self._root)
            self.principal_language = Hl7v2.Field(self._io, self, self._root)


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class UnknownSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = (self._io.read_bytes_full()).decode(u"ASCII")


    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Field(self._io, self, self._root)
            self.value_type = Hl7v2.Field(self._io, self, self._root)
            self.observation_identifier = Hl7v2.Field(self._io, self, self._root)
            self.observation_sub_id = Hl7v2.Field(self._io, self, self._root)
            self.observation_value = Hl7v2.Field(self._io, self, self._root)
            self.units = Hl7v2.Field(self._io, self, self._root)
            self.references_range = Hl7v2.Field(self._io, self, self._root)
            self.abnormal_flags = Hl7v2.Field(self._io, self, self._root)
            self.probability = Hl7v2.Field(self._io, self, self._root)
            self.nature_of_abnormal_test = Hl7v2.Field(self._io, self, self._root)
            self.observation_result_status = Hl7v2.Field(self._io, self, self._root)
            self.effective_date_of_reference_range = Hl7v2.Field(self._io, self, self._root)
            self.user_defined_access_checks = Hl7v2.Field(self._io, self, self._root)
            self.datetime_of_the_observation = Hl7v2.Field(self._io, self, self._root)
            self.producers_id = Hl7v2.Field(self._io, self, self._root)
            self.responsible_observer = Hl7v2.Field(self._io, self, self._root)
            self.observation_method = Hl7v2.Field(self._io, self, self._root)
            self.equipment_instance_identifier = Hl7v2.Field(self._io, self, self._root)
            self.datetime_of_the_analysis = Hl7v2.Field(self._io, self, self._root)


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Field(self._io, self, self._root)
            self.patient_class = Hl7v2.Field(self._io, self, self._root)
            self.assigned_patient_location = Hl7v2.Field(self._io, self, self._root)
            self.admission_type = Hl7v2.Field(self._io, self, self._root)
            self.preadmit_number = Hl7v2.Field(self._io, self, self._root)
            self.prior_patient_location = Hl7v2.Field(self._io, self, self._root)
            self.attending_doctor = Hl7v2.Field(self._io, self, self._root)
            self.referring_doctor = Hl7v2.Field(self._io, self, self._root)
            self.consulting_doctor = Hl7v2.Field(self._io, self, self._root)
            self.hospital_service = Hl7v2.Field(self._io, self, self._root)
            self.temporary_location = Hl7v2.Field(self._io, self, self._root)
            self.preadmit_test_indicator = Hl7v2.Field(self._io, self, self._root)
            self.readmission_indicator = Hl7v2.Field(self._io, self, self._root)
            self.admit_source = Hl7v2.Field(self._io, self, self._root)
            self.ambulatory_status = Hl7v2.Field(self._io, self, self._root)
            self.vip_indicator = Hl7v2.Field(self._io, self, self._root)
            self.admitting_doctor = Hl7v2.Field(self._io, self, self._root)
            self.patient_type = Hl7v2.Field(self._io, self, self._root)
            self.visit_number = Hl7v2.Field(self._io, self, self._root)
            self.financial_class = Hl7v2.Field(self._io, self, self._root)
            self.charge_price_indicator = Hl7v2.Field(self._io, self, self._root)
            self.courtesy_code = Hl7v2.Field(self._io, self, self._root)
            self.credit_rating = Hl7v2.Field(self._io, self, self._root)
            self.contract_code = Hl7v2.Field(self._io, self, self._root)
            self.contract_effective_date = Hl7v2.Field(self._io, self, self._root)
            self.contract_amount = Hl7v2.Field(self._io, self, self._root)
            self.contract_period = Hl7v2.Field(self._io, self, self._root)
            self.interest_code = Hl7v2.Field(self._io, self, self._root)
            self.transfer_to_bad_debt_code = Hl7v2.Field(self._io, self, self._root)
            self.transfer_to_bad_debt_date = Hl7v2.Field(self._io, self, self._root)
            self.bad_debt_agency_code = Hl7v2.Field(self._io, self, self._root)
            self.bad_debt_transfer_amount = Hl7v2.Field(self._io, self, self._root)
            self.bad_debt_recovery_amount = Hl7v2.Field(self._io, self, self._root)
            self.delete_account_indicator = Hl7v2.Field(self._io, self, self._root)
            self.delete_account_date = Hl7v2.Field(self._io, self, self._root)
            self.discharge_disposition = Hl7v2.Field(self._io, self, self._root)
            self.discharged_to_location = Hl7v2.Field(self._io, self, self._root)
            self.diet_type = Hl7v2.Field(self._io, self, self._root)
            self.servicing_facility = Hl7v2.Field(self._io, self, self._root)
            self.bed_status = Hl7v2.Field(self._io, self, self._root)
            self.account_status = Hl7v2.Field(self._io, self, self._root)
            self.pending_location = Hl7v2.Field(self._io, self, self._root)
            self.prior_temporary_location = Hl7v2.Field(self._io, self, self._root)
            self.admit_datetime = Hl7v2.Field(self._io, self, self._root)
            self.discharge_datetime = Hl7v2.Field(self._io, self, self._root)
            self.current_patient_balance = Hl7v2.Field(self._io, self, self._root)
            self.total_charges = Hl7v2.Field(self._io, self, self._root)
            self.total_adjustments = Hl7v2.Field(self._io, self, self._root)
            self.total_payments = Hl7v2.Field(self._io, self, self._root)
            self.alternate_visit_id = Hl7v2.Field(self._io, self, self._root)
            self.visit_indicator = Hl7v2.Field(self._io, self, self._root)
            self.other_healthcare_provider = Hl7v2.Field(self._io, self, self._root)


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
                self.segment_content = Hl7v2.PidSegment(self._io, self, self._root)
            elif _on == u"OBX":
                self.segment_content = Hl7v2.ObxSegment(self._io, self, self._root)
            elif _on == u"PV1":
                self.segment_content = Hl7v2.Pv1Segment(self._io, self, self._root)
            elif _on == u"OBR":
                self.segment_content = Hl7v2.ObrSegment(self._io, self, self._root)
            elif _on == u"MSH":
                self.segment_content = Hl7v2.MshSegment(self._io, self, self._root)
            else:
                self.segment_content = Hl7v2.UnknownSegment(self._io, self, self._root)


    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Field(self._io, self, self._root)
            self.placer_order_number = Hl7v2.Field(self._io, self, self._root)
            self.filler_order_number = Hl7v2.Field(self._io, self, self._root)
            self.universal_service_identifier = Hl7v2.Field(self._io, self, self._root)
            self.priority = Hl7v2.Field(self._io, self, self._root)
            self.requested_datetime = Hl7v2.Field(self._io, self, self._root)
            self.observation_datetime = Hl7v2.Field(self._io, self, self._root)
            self.observation_end_datetime = Hl7v2.Field(self._io, self, self._root)
            self.collection_volume = Hl7v2.Field(self._io, self, self._root)
            self.collector_identifier = Hl7v2.Field(self._io, self, self._root)
            self.specimen_action_code = Hl7v2.Field(self._io, self, self._root)
            self.danger_code = Hl7v2.Field(self._io, self, self._root)
            self.relevant_clinical_info = Hl7v2.Field(self._io, self, self._root)
            self.specimen_received_datetime = Hl7v2.Field(self._io, self, self._root)
            self.specimen_source = Hl7v2.Field(self._io, self, self._root)
            self.ordering_provider = Hl7v2.Field(self._io, self, self._root)
            self.order_callback_phone_number = Hl7v2.Field(self._io, self, self._root)
            self.placers_field_1 = Hl7v2.Field(self._io, self, self._root)
            self.placers_field_2 = Hl7v2.Field(self._io, self, self._root)
            self.filler_field_1 = Hl7v2.Field(self._io, self, self._root)
            self.filler_field_2 = Hl7v2.Field(self._io, self, self._root)
            self.results_rpt_status_chng_datetime = Hl7v2.Field(self._io, self, self._root)
            self.charge_to_practice = Hl7v2.Field(self._io, self, self._root)
            self.diagnostic_serv_sect_id = Hl7v2.Field(self._io, self, self._root)
            self.result_status = Hl7v2.Field(self._io, self, self._root)
            self.parent_result = Hl7v2.Field(self._io, self, self._root)
            self.quantity_timing = Hl7v2.Field(self._io, self, self._root)
            self.result_copies_to = Hl7v2.Field(self._io, self, self._root)
            self.parent = Hl7v2.Field(self._io, self, self._root)
            self.transportation_mode = Hl7v2.Field(self._io, self, self._root)
            self.reason_for_study = Hl7v2.Field(self._io, self, self._root)
            self.principal_result_interpreter = Hl7v2.Field(self._io, self, self._root)
            self.assistant_result_interpreter = Hl7v2.Field(self._io, self, self._root)
            self.technician = Hl7v2.Field(self._io, self, self._root)
            self.transcriptionist = Hl7v2.Field(self._io, self, self._root)
            self.scheduled_datetime = Hl7v2.Field(self._io, self, self._root)
            self.number_of_sample_containers = Hl7v2.Field(self._io, self, self._root)
            self.transport_logistics_of_sample = Hl7v2.Field(self._io, self, self._root)
            self.collectors_comment = Hl7v2.Field(self._io, self, self._root)
            self.transport_arrangement_responsibility = Hl7v2.Field(self._io, self, self._root)
            self.transport_arranged = Hl7v2.Field(self._io, self, self._root)
            self.escort_required = Hl7v2.Field(self._io, self, self._root)
            self.planned_patient_transport_comment = Hl7v2.Field(self._io, self, self._root)
            self.procedure_code = Hl7v2.Field(self._io, self, self._root)
            self.procedure_code_modifier = Hl7v2.Field(self._io, self, self._root)
            self.placer_supplemental_service_info = Hl7v2.Field(self._io, self, self._root)
            self.filler_supplemental_service_info = Hl7v2.Field(self._io, self, self._root)
            self.medically_necessary_duplicate_proc_reason = Hl7v2.Field(self._io, self, self._root)
            self.result_handling = Hl7v2.Field(self._io, self, self._root)


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = Hl7v2.Field(self._io, self, self._root)
            self.patient_id = Hl7v2.Field(self._io, self, self._root)
            self.patient_identifier_list = Hl7v2.Field(self._io, self, self._root)
            self.alternate_patient_id = Hl7v2.Field(self._io, self, self._root)
            self.patient_name = Hl7v2.Field(self._io, self, self._root)
            self.mothers_maiden_name = Hl7v2.Field(self._io, self, self._root)
            self.date_of_birth = Hl7v2.Field(self._io, self, self._root)
            self.administrative_sex = Hl7v2.Field(self._io, self, self._root)
            self.patient_alias = Hl7v2.Field(self._io, self, self._root)
            self.race = Hl7v2.Field(self._io, self, self._root)
            self.patient_address = Hl7v2.Field(self._io, self, self._root)
            self.county_code = Hl7v2.Field(self._io, self, self._root)
            self.phone_number_home = Hl7v2.Field(self._io, self, self._root)
            self.phone_number_business = Hl7v2.Field(self._io, self, self._root)
            self.primary_language = Hl7v2.Field(self._io, self, self._root)
            self.marital_status = Hl7v2.Field(self._io, self, self._root)
            self.religion = Hl7v2.Field(self._io, self, self._root)
            self.patient_account_number = Hl7v2.Field(self._io, self, self._root)
            self.ssn_number = Hl7v2.Field(self._io, self, self._root)
            self.drivers_license = Hl7v2.Field(self._io, self, self._root)
            self.mothers_identifier = Hl7v2.Field(self._io, self, self._root)
            self.ethnic_group = Hl7v2.Field(self._io, self, self._root)
            self.birth_place = Hl7v2.Field(self._io, self, self._root)
            self.multiple_birth_indicator = Hl7v2.Field(self._io, self, self._root)
            self.birth_order = Hl7v2.Field(self._io, self, self._root)
            self.citizenship = Hl7v2.Field(self._io, self, self._root)
            self.veterans_military_status = Hl7v2.Field(self._io, self, self._root)
            self.nationality = Hl7v2.Field(self._io, self, self._root)
            self.patient_death_date_time = Hl7v2.Field(self._io, self, self._root)
            self.patient_death_indicator = Hl7v2.Field(self._io, self, self._root)


    class Component(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.parts = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")



