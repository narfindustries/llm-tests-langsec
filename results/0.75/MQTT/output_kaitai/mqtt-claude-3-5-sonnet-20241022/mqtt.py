# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Mqtt(KaitaiStruct):

    class MsgType(Enum):
        reserved = 0
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
        reserved2 = 15
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.fixed_header = Mqtt.FixedHeader(self._io, self, self._root)
        if self.fixed_header.msg_type != Mqtt.MsgType.disconnect:
            self.variable_header = Mqtt.VariableHeader(self._io, self, self._root)

        if  ((self.fixed_header.msg_type != Mqtt.MsgType.disconnect) and (self.fixed_header.msg_type != Mqtt.MsgType.pingreq) and (self.fixed_header.msg_type != Mqtt.MsgType.pingresp)) :
            self.payload = Mqtt.Payload(self._io, self, self._root)


    class VlqInt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte1 = self._io.read_u1()
            if self.byte1 >= 128:
                self.byte2 = self._io.read_u1()

            if self.byte2 >= 128:
                self.byte3 = self._io.read_u1()

            if self.byte3 >= 128:
                self.byte4 = self._io.read_u1()


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.byte1 & (((127 + (((self.byte2 & 127) << 7) if self.byte2 >= 128 else 0)) + (((self.byte3 & 127) << 14) if self.byte3 >= 128 else 0)) + (((self.byte4 & 127) << 21) if self.byte4 >= 128 else 0)))
            return getattr(self, '_m_value', None)


    class Payload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.fixed_header.msg_type == Mqtt.MsgType.connect:
                self.client_id = Mqtt.MqttString(self._io, self, self._root)

            if self._parent.fixed_header.msg_type == Mqtt.MsgType.publish:
                self.topic_name = Mqtt.MqttString(self._io, self, self._root)

            if self._parent.fixed_header.msg_type == Mqtt.MsgType.publish:
                self.message = (self._io.read_bytes_full()).decode(u"utf8")



    class FixedHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.control_byte = self._io.read_u1()
            self.remaining_length = Mqtt.VlqInt(self._io, self, self._root)

        @property
        def msg_type(self):
            if hasattr(self, '_m_msg_type'):
                return self._m_msg_type

            self._m_msg_type = KaitaiStream.resolve_enum(Mqtt.MsgType, ((self.control_byte & 240) >> 4))
            return getattr(self, '_m_msg_type', None)

        @property
        def flags(self):
            if hasattr(self, '_m_flags'):
                return self._m_flags

            self._m_flags = (self.control_byte & 15)
            return getattr(self, '_m_flags', None)


    class VariableHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.fixed_header.msg_type == Mqtt.MsgType.connect:
                self.protocol_name = Mqtt.MqttString(self._io, self, self._root)

            if self._parent.fixed_header.msg_type == Mqtt.MsgType.connect:
                self.protocol_version = self._io.read_u1()

            if self._parent.fixed_header.msg_type == Mqtt.MsgType.connect:
                self.connect_flags = self._io.read_u1()

            if self._parent.fixed_header.msg_type == Mqtt.MsgType.connect:
                self.keep_alive = self._io.read_u2be()

            if  ((self._parent.fixed_header.msg_type == Mqtt.MsgType.publish) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.puback) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.pubrec) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.pubrel) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.pubcomp) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.subscribe) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.suback) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.unsubscribe) or (self._parent.fixed_header.msg_type == Mqtt.MsgType.unsuback)) :
                self.packet_identifier = self._io.read_u2be()



    class MqttString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.len)).decode(u"utf8")



