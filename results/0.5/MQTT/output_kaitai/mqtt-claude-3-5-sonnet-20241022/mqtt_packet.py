# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class MqttPacket(KaitaiStruct):

    class MessageType(Enum):
        connect = 1
        connack = 2
        publish = 3
        puback = 4
        pubrec = 5
        pubrel = 6
        pubcomp = 7
        subscribe = 8
        suback = 9
        unsubscribe = 10
        unsuback = 11
        pingreq = 12
        pingresp = 13
        disconnect = 14
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.fixed_header = MqttPacket.FixedHeader(self._io, self, self._root)
        if  ((self.fixed_header.message_type != MqttPacket.MessageType.pingreq) and (self.fixed_header.message_type != MqttPacket.MessageType.pingresp)) :
            self.variable_header = MqttPacket.VariableHeader(self._io, self, self._root)

        if self.has_payload:
            self.payload = MqttPacket.Payload(self._io, self, self._root)


    class Payload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class FixedHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.message_type_and_flags = self._io.read_u1()
            self.remaining_length = MqttPacket.RemainingLength(self._io, self, self._root)

        @property
        def message_type(self):
            if hasattr(self, '_m_message_type'):
                return self._m_message_type

            self._m_message_type = KaitaiStream.resolve_enum(MqttPacket.MessageType, ((self.message_type_and_flags & 240) >> 4))
            return getattr(self, '_m_message_type', None)

        @property
        def flags(self):
            if hasattr(self, '_m_flags'):
                return self._m_flags

            self._m_flags = (self.message_type_and_flags & 15)
            return getattr(self, '_m_flags', None)


    class RemainingLength(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.bytes = []
            i = 0
            while True:
                _ = self._io.read_u1()
                self.bytes.append(_)
                if (self.bytes[-1] & 128) == 0:
                    break
                i += 1

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((((self.bytes[0] & 127) + (((self.bytes[1] & 127) << 7) if len(self.bytes) >= 2 else 0)) + (((self.bytes[2] & 127) << 14) if len(self.bytes) >= 3 else 0)) + (((self.bytes[3] & 127) << 21) if len(self.bytes) >= 4 else 0))
            return getattr(self, '_m_value', None)


    class VariableHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.fixed_header.message_type == MqttPacket.MessageType.connect:
                self.protocol_name = MqttPacket.MqttString(self._io, self, self._root)

            if self._parent.fixed_header.message_type == MqttPacket.MessageType.connect:
                self.protocol_level = self._io.read_u1()

            if self._parent.fixed_header.message_type == MqttPacket.MessageType.connect:
                self.connect_flags = self._io.read_u1()

            if self._parent.fixed_header.message_type == MqttPacket.MessageType.connect:
                self.keep_alive = self._io.read_u2be()

            if self.needs_packet_identifier:
                self.packet_identifier = self._io.read_u2be()

            if self._parent.fixed_header.message_type == MqttPacket.MessageType.connack:
                self.return_code = self._io.read_u1()


        @property
        def needs_packet_identifier(self):
            if hasattr(self, '_m_needs_packet_identifier'):
                return self._m_needs_packet_identifier

            self._m_needs_packet_identifier =  ((self._parent.fixed_header.message_type == MqttPacket.MessageType.puback) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.pubrec) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.pubrel) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.pubcomp) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.subscribe) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.suback) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.unsubscribe) or (self._parent.fixed_header.message_type == MqttPacket.MessageType.unsuback)) 
            return getattr(self, '_m_needs_packet_identifier', None)


    class MqttString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.len)).decode(u"UTF-8")


    @property
    def has_payload(self):
        if hasattr(self, '_m_has_payload'):
            return self._m_has_payload

        self._m_has_payload =  ((self.fixed_header.message_type == MqttPacket.MessageType.publish) or (self.fixed_header.message_type == MqttPacket.MessageType.subscribe) or (self.fixed_header.message_type == MqttPacket.MessageType.suback) or (self.fixed_header.message_type == MqttPacket.MessageType.unsubscribe)) 
        return getattr(self, '_m_has_payload', None)


