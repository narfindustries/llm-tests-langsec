# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2(KaitaiStruct):
    """HL7 Version 2.x is a specification for interchange of healthcare data between systems.
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


    class Obx(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.value_type = (self._io.read_bytes(2)).decode(u"ASCII")
            self.observation_identifier = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.observation_value = (self._io.read_bytes(250)).decode(u"ASCII")
            self.units = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.references_range = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")


    class Msh(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"ASCII")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"ASCII")
            self.sending_application = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.sending_facility = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.receiving_application = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.receiving_facility = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.datetime_of_message = (self._io.read_bytes(14)).decode(u"ASCII")
            self.security = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.message_type = (self._io.read_bytes(7)).decode(u"ASCII")
            self.message_control_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.processing_id = (self._io.read_bytes(3)).decode(u"ASCII")
            self.version_id = (self._io.read_bytes(5)).decode(u"ASCII")


    class Orc(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.order_control = (self._io.read_bytes(2)).decode(u"ASCII")
            self.placer_order_number = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.seg_type = (self._io.read_bytes(3)).decode(u"ASCII")

        @property
        def msh_segment(self):
            if hasattr(self, '_m_msh_segment'):
                return self._m_msh_segment

            if self.seg_type == u"MSH":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_msh_segment = Hl7V2.Msh(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_msh_segment', None)

        @property
        def pid_segment(self):
            if hasattr(self, '_m_pid_segment'):
                return self._m_pid_segment

            if self.seg_type == u"PID":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_pid_segment = Hl7V2.Pid(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_pid_segment', None)

        @property
        def orc_segment(self):
            if hasattr(self, '_m_orc_segment'):
                return self._m_orc_segment

            if self.seg_type == u"ORC":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_orc_segment = Hl7V2.Orc(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_orc_segment', None)

        @property
        def obx_segment(self):
            if hasattr(self, '_m_obx_segment'):
                return self._m_obx_segment

            if self.seg_type == u"OBX":
                _pos = self._io.pos()
                self._io.seek(0)
                self._m_obx_segment = Hl7V2.Obx(self._io, self, self._root)
                self._io.seek(_pos)

            return getattr(self, '_m_obx_segment', None)


    class Pid(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.set_id = (self._io.read_bytes(4)).decode(u"ASCII")
            self.patient_id = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.patient_name = (self._io.read_bytes(250)).decode(u"ASCII")
            self.date_of_birth = (self._io.read_bytes(8)).decode(u"ASCII")
            self.sex = (self._io.read_bytes(1)).decode(u"ASCII")
            self.patient_address = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.phone_number_home = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")
            self.phone_number_business = (KaitaiStream.bytes_terminate(self._io.read_bytes(0), 124, False)).decode(u"ASCII")



