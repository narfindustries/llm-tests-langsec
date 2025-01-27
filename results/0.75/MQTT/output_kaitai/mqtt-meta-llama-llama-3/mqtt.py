# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Mqtt(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Mqtt.MqttHeader(self._io, self, self._root)

    class MqttHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte_1 = self._io.read_u1()
            self.remaining_length = self._io.read_u1()

        @property
        def dup_flag(self):
            if hasattr(self, '_m_dup_flag'):
                return self._m_dup_flag

            self._m_dup_flag = ((self.byte_1 & 8) >> 3)
            return getattr(self, '_m_dup_flag', None)

        @property
        def retain_flag(self):
            if hasattr(self, '_m_retain_flag'):
                return self._m_retain_flag

            self._m_retain_flag = (self.byte_1 & 1)
            return getattr(self, '_m_retain_flag', None)

        @property
        def qos_level(self):
            if hasattr(self, '_m_qos_level'):
                return self._m_qos_level

            self._m_qos_level = ((self.byte_1 & 3) >> 1)
            return getattr(self, '_m_qos_level', None)

        @property
        def packet_type(self):
            if hasattr(self, '_m_packet_type'):
                return self._m_packet_type

            self._m_packet_type = (self.byte_1 & 240)
            return getattr(self, '_m_packet_type', None)

        @property
        def payload_len(self):
            if hasattr(self, '_m_payload_len'):
                return self._m_payload_len

            self._m_payload_len = self.remaining_length
            return getattr(self, '_m_payload_len', None)



