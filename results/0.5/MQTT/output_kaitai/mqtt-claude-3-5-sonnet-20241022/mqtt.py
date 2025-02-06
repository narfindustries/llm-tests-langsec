# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Mqtt(KaitaiStruct):

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

    class PropertyIds(Enum):
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
        request_problem_information = 23
        will_delay_interval = 24
        request_response_information = 25
        response_information = 26
        server_reference = 28
        reason_string = 31
        receive_maximum = 33
        topic_alias_maximum = 34
        topic_alias = 35
        maximum_qos = 36
        retain_available = 37
        user_property = 38
        maximum_packet_size = 39
        wildcard_subscription_available = 40
        subscription_identifier_available = 41
        shared_subscription_available = 42
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.fixed_header = Mqtt.FixedHeader(self._io, self, self._root)
        self._raw_variable_header = self._io.read_bytes(self.fixed_header.remaining_length.value)
        _io__raw_variable_header = KaitaiStream(BytesIO(self._raw_variable_header))
        self.variable_header = Mqtt.VariableHeader(_io__raw_variable_header, self, self._root)

    class Disconnect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class UserProperty(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key = Mqtt.MqttString(self._io, self, self._root)
            self.value = Mqtt.MqttString(self._io, self, self._root)


    class Pubcomp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class Connect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.protocol_name = Mqtt.MqttString(self._io, self, self._root)
            self.protocol_version = self._io.read_u1()
            self.connect_flags = self._io.read_u1()
            self.keep_alive = self._io.read_u2be()
            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.client_id = Mqtt.MqttString(self._io, self, self._root)
            if (self.connect_flags & 4) != 0:
                self.will_properties = Mqtt.Properties(self._io, self, self._root)

            if (self.connect_flags & 4) != 0:
                self.will_topic = Mqtt.MqttString(self._io, self, self._root)

            if (self.connect_flags & 4) != 0:
                self.will_payload = Mqtt.BinaryData(self._io, self, self._root)

            if (self.connect_flags & 128) != 0:
                self.username = Mqtt.MqttString(self._io, self, self._root)

            if (self.connect_flags & 64) != 0:
                self.password = Mqtt.MqttString(self._io, self, self._root)



    class Auth(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class FixedHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_type_and_flags = self._io.read_u1()
            self.remaining_length = Mqtt.Vlq(self._io, self, self._root)

        @property
        def packet_type(self):
            if hasattr(self, '_m_packet_type'):
                return self._m_packet_type

            self._m_packet_type = KaitaiStream.resolve_enum(Mqtt.PacketTypes, ((self.packet_type_and_flags & 240) >> 4))
            return getattr(self, '_m_packet_type', None)

        @property
        def flags(self):
            if hasattr(self, '_m_flags'):
                return self._m_flags

            self._m_flags = (self.packet_type_and_flags & 15)
            return getattr(self, '_m_flags', None)


    class Pubrel(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class Subscription(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_filter = Mqtt.MqttString(self._io, self, self._root)
            self.options = self._io.read_u1()


    class Suback(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.reason_codes = []
            i = 0
            while not self._io.is_eof():
                self.reason_codes.append(self._io.read_u1())
                i += 1



    class Subscribe(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.subscriptions = []
            i = 0
            while not self._io.is_eof():
                self.subscriptions.append(Mqtt.Subscription(self._io, self, self._root))
                i += 1



    class Pubrec(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class Publish(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_name = Mqtt.MqttString(self._io, self, self._root)
            if (self._parent._parent.fixed_header.flags & 6) != 0:
                self.packet_identifier = self._io.read_u2be()

            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.payload = self._io.read_bytes_full()


    class Properties(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = Mqtt.Vlq(self._io, self, self._root)
            self.properties = []
            i = 0
            while not self._io.is_eof():
                self.properties.append(Mqtt.Property(self._io, self, self._root))
                i += 1



    class Unsuback(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.reason_codes = []
            i = 0
            while not self._io.is_eof():
                self.reason_codes.append(self._io.read_u1())
                i += 1



    class Unsubscribe(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.properties = Mqtt.Properties(self._io, self, self._root)
            self.topic_filters = []
            i = 0
            while not self._io.is_eof():
                self.topic_filters.append(Mqtt.MqttString(self._io, self, self._root))
                i += 1



    class VariableHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            _on = self._parent.fixed_header.packet_type
            if _on == Mqtt.PacketTypes.pubrel:
                self.content = Mqtt.Pubrel(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.unsubscribe:
                self.content = Mqtt.Unsubscribe(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.disconnect:
                self.content = Mqtt.Disconnect(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.subscribe:
                self.content = Mqtt.Subscribe(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.auth:
                self.content = Mqtt.Auth(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.suback:
                self.content = Mqtt.Suback(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.connect:
                self.content = Mqtt.Connect(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.publish:
                self.content = Mqtt.Publish(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.connack:
                self.content = Mqtt.Connack(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.pubcomp:
                self.content = Mqtt.Pubcomp(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.unsuback:
                self.content = Mqtt.Unsuback(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.puback:
                self.content = Mqtt.Puback(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.pubrec:
                self.content = Mqtt.Pubrec(self._io, self, self._root)


    class BinaryData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_data = self._io.read_u2be()
            self.data = self._io.read_bytes(self.len_data)


    class MqttString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_str = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.len_str)).decode(u"UTF-8")


    class Property(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = KaitaiStream.resolve_enum(Mqtt.PropertyIds, self._io.read_u1())
            _on = self.identifier
            if _on == Mqtt.PropertyIds.authentication_data:
                self.value = Mqtt.BinaryData(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.content_type:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.subscription_identifier_available:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.server_reference:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.response_information:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.maximum_qos:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.will_delay_interval:
                self.value = self._io.read_u4be()
            elif _on == Mqtt.PropertyIds.session_expiry_interval:
                self.value = self._io.read_u4be()
            elif _on == Mqtt.PropertyIds.maximum_packet_size:
                self.value = self._io.read_u4be()
            elif _on == Mqtt.PropertyIds.topic_alias_maximum:
                self.value = self._io.read_u2be()
            elif _on == Mqtt.PropertyIds.wildcard_subscription_available:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.shared_subscription_available:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.message_expiry_interval:
                self.value = self._io.read_u4be()
            elif _on == Mqtt.PropertyIds.user_property:
                self.value = Mqtt.UserProperty(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.topic_alias:
                self.value = self._io.read_u2be()
            elif _on == Mqtt.PropertyIds.request_problem_information:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.authentication_method:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.assigned_client_identifier:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.payload_format_indicator:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.subscription_identifier:
                self.value = Mqtt.Vlq(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.retain_available:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.correlation_data:
                self.value = Mqtt.BinaryData(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.receive_maximum:
                self.value = self._io.read_u2be()
            elif _on == Mqtt.PropertyIds.server_keep_alive:
                self.value = self._io.read_u2be()
            elif _on == Mqtt.PropertyIds.reason_string:
                self.value = Mqtt.MqttString(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.request_response_information:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.response_topic:
                self.value = Mqtt.MqttString(self._io, self, self._root)


    class Connack(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.acknowledge_flags = self._io.read_u1()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class Puback(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_identifier = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Properties(self._io, self, self._root)


    class Vlq(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups = []
            i = 0
            while True:
                _ = self._io.read_u1()
                self.groups.append(_)
                if (_ & 128) == 0:
                    break
                i += 1

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.groups[0] & (((127 + ((self.groups[1] & 127) * 128)) + ((self.groups[2] & 127) * 16384)) + ((self.groups[3] & 127) * 2097152)))
            return getattr(self, '_m_value', None)



