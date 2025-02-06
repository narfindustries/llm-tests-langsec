# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):

    class SexEnum(Enum):
        ambiguous = 65
        female = 70
        male = 77
        other = 79
        unknown = 85
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
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = Hl7V2.HdType(self._io, self, self._root)
            self.sending_facility = Hl7V2.HdType(self._io, self, self._root)
            self.receiving_application = Hl7V2.HdType(self._io, self, self._root)
            self.receiving_facility = Hl7V2.HdType(self._io, self, self._root)
            self.datetime = Hl7V2.TsType(self._io, self, self._root)
            self.security = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.message_type = Hl7V2.MsgType(self._io, self, self._root)
            self.message_control_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.processing_id = Hl7V2.PtType(self._io, self, self._root)
            self.version_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class UnknownSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.content = self._io.read_bytes_full()


    class TsType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.time = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class MsgType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.message_code = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.trigger_event = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.message_structure = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = self._io.read_u1()
            self.patient_class = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.assigned_patient_location = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.admission_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.preadmit_number = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.prior_patient_location = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_id = (self._io.read_bytes(3)).decode(u"ASCII")
            self.field_separator = self._io.read_u1()
            _on = self.segment_id
            if _on == u"MSH":
                self.content = Hl7V2.MshSegment(self._io, self, self._root)
            elif _on == u"PID":
                self.content = Hl7V2.PidSegment(self._io, self, self._root)
            elif _on == u"PV1":
                self.content = Hl7V2.Pv1Segment(self._io, self, self._root)
            else:
                self.content = Hl7V2.UnknownSegment(self._io, self, self._root)


    class HdType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.namespace_id = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.universal_id = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.universal_id_type = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class PtType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.processing_id = (self._io.read_bytes_term(94, False, True, True)).decode(u"ASCII")
            self.processing_mode = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = self._io.read_u1()
            self.patient_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_identifier_list = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.alternate_patient_id = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_name = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.mothers_maiden_name = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.datetime_of_birth = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.sex = KaitaiStream.resolve_enum(Hl7V2.SexEnum, self._io.read_u1())
            self.patient_alias = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.race = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.patient_address = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")
            self.county_code = (self._io.read_bytes_term(124, False, True, True)).decode(u"ASCII")



