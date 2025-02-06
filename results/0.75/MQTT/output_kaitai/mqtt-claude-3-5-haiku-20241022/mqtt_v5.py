# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class MqttV5(KaitaiStruct):

    class PacketTypes(Enum):
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
        auth = 15

    class PropertyIdentifiers(Enum):
        payload_format_indicator = 1
        message_expiry_interval = 2
        content_type = 3
        response_topic = 8
        correlation_data = 9
        subscription_identifier = 11
        session_expiry_interval = 17
        assigned_client_identifier = 18
        server_keep_alive = 19
        authentication_method = 21
        authentication_data = 22
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.packet_type = KaitaiStream.resolve_enum(MqttV5.PacketTypes, self._io.read_bits_int_be(4))
        self.flags = self._io.read_bits_int_be(4)
        self._io.align_to_byte()
        self.remaining_length = MqttV5.VlqBase128Le(self._io, self, self._root)

    class PrefixedString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.length)).decode(u"utf-8")


    class VlqBase128Le(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups = []
            i = 0
            while True:
                _ = MqttV5.VlqGroup(self._io, self, self._root)
                self.groups.append(_)
                if not (_.has_next):
                    break
                i += 1

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (((self.groups[0].value + ((self.groups[1].value << 7) if len(self.groups) >= 2 else 0)) + ((self.groups[2].value << 14) if len(self.groups) >= 3 else 0)) + ((self.groups[3].value << 21) if len(self.groups) >= 4 else 0))
            return getattr(self, '_m_value', None)


    class PropertiesList(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = MqttV5.VlqBase128Le(self._io, self, self._root)
            self.properties = []
            for i in range(self.length.value):
                self.properties.append(MqttV5.Property(self._io, self, self._root))



    class ConnectFlags(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_bits_int_be(1) != 0
            self.clean_start = self._io.read_bits_int_be(1) != 0
            self.will_flag = self._io.read_bits_int_be(1) != 0
            self.will_qos = self._io.read_bits_int_be(2)
            self.will_retain = self._io.read_bits_int_be(1) != 0
            self.password_flag = self._io.read_bits_int_be(1) != 0
            self.username_flag = self._io.read_bits_int_be(1) != 0


    class ConnectPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.protocol_name = (KaitaiStream.bytes_terminate(self._io.read_bytes(4), 0, False)).decode(u"ascii")
            self.protocol_version = self._io.read_u1()
            if not self.protocol_version == 5:
                raise kaitaistruct.ValidationNotEqualError(5, self.protocol_version, self._io, u"/types/connect_packet/seq/1")
            self.connect_flags = MqttV5.ConnectFlags(self._io, self, self._root)
            self.keep_alive = self._io.read_u2be()
            self.properties = MqttV5.PropertiesList(self._io, self, self._root)
            self.client_identifier = MqttV5.PrefixedString(self._io, self, self._root)
            if self.connect_flags.will_flag:
                self.will_properties = MqttV5.PropertiesList(self._io, self, self._root)

            if self.connect_flags.will_flag:
                self.will_topic = MqttV5.PrefixedString(self._io, self, self._root)

            if self.connect_flags.will_flag:
                self.will_payload = MqttV5.PrefixedByteArray(self.will_topic.length, self._io, self, self._root)

            if self.connect_flags.username_flag:
                self.username = MqttV5.PrefixedString(self._io, self, self._root)

            if self.connect_flags.password_flag:
                self.password = MqttV5.PrefixedString(self._io, self, self._root)



    class PrefixedByteArray(KaitaiStruct):
        def __init__(self, num_value, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self.num_value = num_value
            self._read()

        def _read(self):
            self.value = []
            for i in range(self.num_value):
                self.value.append(self._io.read_u1())



    class Property(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = KaitaiStream.resolve_enum(MqttV5.PropertyIdentifiers, self._io.read_u1())
            _on = self.identifier
            if _on == MqttV5.PropertyIdentifiers.correlation_data:
                self.value = MqttV5.PrefixedByteArray(0, self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.content_type:
                self.value = MqttV5.PrefixedString(self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.response_topic:
                self.value = MqttV5.PrefixedString(self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.subscription_identifier:
                self.value = MqttV5.VlqBase128Le(self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.authentication_data:
                self.value = MqttV5.PrefixedByteArray(0, self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.assigned_client_identifier:
                self.value = MqttV5.PrefixedString(self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.message_expiry_interval:
                self.value = self._io.read_u4be()
            elif _on == MqttV5.PropertyIdentifiers.authentication_method:
                self.value = MqttV5.PrefixedString(self._io, self, self._root)
            elif _on == MqttV5.PropertyIdentifiers.server_keep_alive:
                self.value = self._io.read_u2be()
            elif _on == MqttV5.PropertyIdentifiers.session_expiry_interval:
                self.value = self._io.read_u4be()
            elif _on == MqttV5.PropertyIdentifiers.payload_format_indicator:
                self.value = self._io.read_u1()


    class VlqGroup(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.has_next = self._io.read_bits_int_be(1) != 0
            self.value = self._io.read_bits_int_be(7)



