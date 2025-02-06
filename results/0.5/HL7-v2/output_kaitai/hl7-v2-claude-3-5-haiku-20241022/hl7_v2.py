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
        self.message = Hl7V2.Hl7Message(self._io, self, self._root)

    class MshSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"ascii")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ascii")
            self.sending_application = (self._io.read_bytes_full()).decode(u"ascii")
            self.sending_facility = (self._io.read_bytes_full()).decode(u"ascii")
            self.receiving_application = (self._io.read_bytes_full()).decode(u"ascii")
            self.receiving_facility = (self._io.read_bytes_full()).decode(u"ascii")
            self.timestamp = (self._io.read_bytes_full()).decode(u"ascii")
            self.security = (self._io.read_bytes_full()).decode(u"ascii")
            self.message_type = (self._io.read_bytes_full()).decode(u"ascii")
            self.message_control_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.processing_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.version = (self._io.read_bytes_full()).decode(u"ascii")


    class Hl7Message(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.msh_segment = Hl7V2.MshSegment(self._io, self, self._root)
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
            self.set_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.value_type = (self._io.read_bytes_full()).decode(u"ascii")
            self.observation_identifier = (self._io.read_bytes_full()).decode(u"ascii")
            self.observation_value = (self._io.read_bytes_full()).decode(u"ascii")
            self.units = (self._io.read_bytes_full()).decode(u"ascii")


    class Pv1Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.patient_class = (self._io.read_bytes_full()).decode(u"ascii")
            self.assigned_patient_location = (self._io.read_bytes_full()).decode(u"ascii")
            self.admission_type = (self._io.read_bytes_full()).decode(u"ascii")
            self.preadmit_number = (self._io.read_bytes_full()).decode(u"ascii")


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._io.pos() < self._io.size():
                self.pid_segment = Hl7V2.PidSegment(self._io, self, self._root)

            if self._io.pos() < self._io.size():
                self.pv1_segment = Hl7V2.Pv1Segment(self._io, self, self._root)

            if self._io.pos() < self._io.size():
                self.obr_segment = Hl7V2.ObrSegment(self._io, self, self._root)

            if self._io.pos() < self._io.size():
                self.obx_segment = Hl7V2.ObxSegment(self._io, self, self._root)



    class ObrSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.placer_order_number = (self._io.read_bytes_full()).decode(u"ascii")
            self.filler_order_number = (self._io.read_bytes_full()).decode(u"ascii")
            self.universal_service_identifier = (self._io.read_bytes_full()).decode(u"ascii")
            self.observation_date_time = (self._io.read_bytes_full()).decode(u"ascii")


    class PidSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.patient_id = (self._io.read_bytes_full()).decode(u"ascii")
            self.patient_name = (self._io.read_bytes_full()).decode(u"ascii")
            self.mother_maiden_name = (self._io.read_bytes_full()).decode(u"ascii")
            self.date_of_birth = (self._io.read_bytes_full()).decode(u"ascii")
            self.sex = (self._io.read_bytes_full()).decode(u"ascii")
            self.patient_race = (self._io.read_bytes_full()).decode(u"ascii")



