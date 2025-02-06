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
            self.sending_application = (self._io.read_bytes(227)).decode(u"ASCII")
            self.sending_facility = (self._io.read_bytes(227)).decode(u"ASCII")
            self.receiving_application = (self._io.read_bytes(227)).decode(u"ASCII")
            self.receiving_facility = (self._io.read_bytes(227)).decode(u"ASCII")
            self.datetime = (self._io.read_bytes(26)).decode(u"ASCII")
            self.security = (self._io.read_bytes(40)).decode(u"ASCII")
            self.message_type = (self._io.read_bytes(15)).decode(u"ASCII")
            self.message_control_id = (self._io.read_bytes(20)).decode(u"ASCII")
            self.processing_id = (self._io.read_bytes(3)).decode(u"ASCII")
            self.version_id = (self._io.read_bytes(60)).decode(u"ASCII")
            self.sequence_number = (self._io.read_bytes(15)).decode(u"ASCII")
            self.continuation_pointer = (self._io.read_bytes(180)).decode(u"ASCII")
            self.accept_ack_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.application_ack_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.country_code = (self._io.read_bytes(3)).decode(u"ASCII")
            self.character_set = (self._io.read_bytes(16)).decode(u"ASCII")
            self.principal_language = (self._io.read_bytes(250)).decode(u"ASCII")


    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"ASCII")
            self.field_content = (self._io.read_bytes_full()).decode(u"ASCII")


    class ObxSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.value_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.observation_identifier = (self._io.read_bytes(250)).decode(u"ASCII")
            self.observation_sub_id = (self._io.read_bytes(20)).decode(u"ASCII")
            self.observation_value = (self._io.read_bytes(250)).decode(u"ASCII")
            self.units = (self._io.read_bytes(250)).decode(u"ASCII")
            self.references_range = (self._io.read_bytes(60)).decode(u"ASCII")
            self.abnormal_flags = (self._io.read_bytes(5)).decode(u"ASCII")
            self.probability = (self._io.read_bytes(5)).decode(u"ASCII")
            self.nature_of_abnormal_test = (self._io.read_bytes(2)).decode(u"ASCII")
            self.observation_result_status = (self._io.read_bytes(1)).decode(u"ASCII")
            self.effective_date_last_normal = (self._io.read_bytes(26)).decode(u"ASCII")


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_type = (self._io.read_bytes(3)).decode(u"ASCII")
            self.fields = []
            i = 0
            while True:
                _ = Hl7V2.Field(self._io, self, self._root)
                self.fields.append(_)
                if _.field_content == u"\r":
                    break
                i += 1


    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.placer_order_number = (self._io.read_bytes(75)).decode(u"ASCII")
            self.filler_order_number = (self._io.read_bytes(75)).decode(u"ASCII")
            self.universal_service_id = (self._io.read_bytes(250)).decode(u"ASCII")
            self.priority = (self._io.read_bytes(2)).decode(u"ASCII")
            self.requested_datetime = (self._io.read_bytes(26)).decode(u"ASCII")
            self.observation_datetime = (self._io.read_bytes(26)).decode(u"ASCII")
            self.observation_end_datetime = (self._io.read_bytes(26)).decode(u"ASCII")
            self.collection_volume = (self._io.read_bytes(20)).decode(u"ASCII")
            self.collector_identifier = (self._io.read_bytes(250)).decode(u"ASCII")
            self.specimen_action_code = (self._io.read_bytes(1)).decode(u"ASCII")
            self.danger_code = (self._io.read_bytes(250)).decode(u"ASCII")


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.patient_id = (self._io.read_bytes(20)).decode(u"ASCII")
            self.patient_identifier_list = (self._io.read_bytes(250)).decode(u"ASCII")
            self.alternate_patient_id = (self._io.read_bytes(20)).decode(u"ASCII")
            self.patient_name = (self._io.read_bytes(250)).decode(u"ASCII")
            self.mothers_maiden_name = (self._io.read_bytes(250)).decode(u"ASCII")
            self.date_of_birth = (self._io.read_bytes(26)).decode(u"ASCII")
            self.sex = (self._io.read_bytes(1)).decode(u"ASCII")
            self.patient_alias = (self._io.read_bytes(250)).decode(u"ASCII")
            self.race = (self._io.read_bytes(250)).decode(u"ASCII")
            self.patient_address = (self._io.read_bytes(250)).decode(u"ASCII")
            self.county_code = (self._io.read_bytes(4)).decode(u"ASCII")
            self.phone_number_home = (self._io.read_bytes(250)).decode(u"ASCII")
            self.phone_number_business = (self._io.read_bytes(250)).decode(u"ASCII")
            self.primary_language = (self._io.read_bytes(250)).decode(u"ASCII")
            self.marital_status = (self._io.read_bytes(250)).decode(u"ASCII")
            self.religion = (self._io.read_bytes(250)).decode(u"ASCII")
            self.patient_account_number = (self._io.read_bytes(20)).decode(u"ASCII")
            self.ssn = (self._io.read_bytes(16)).decode(u"ASCII")
            self.drivers_license = (self._io.read_bytes(25)).decode(u"ASCII")



