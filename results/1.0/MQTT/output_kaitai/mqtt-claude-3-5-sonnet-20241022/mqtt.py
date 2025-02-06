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

    class ReasonCodes(Enum):
        success = 0
        normal_disconnection = 1
        granted_qos0 = 2
        granted_qos1 = 3
        granted_qos2 = 4
        unspecified_error = 128
        malformed_packet = 129
        protocol_error = 130
        implementation_specific_error = 131
        unsupported_protocol_version = 132
        client_identifier_not_valid = 133
        bad_username_or_password = 134
        not_authorized = 135
        server_unavailable = 136
        server_busy = 137
        banned = 138
        server_shutting_down = 139
        bad_authentication_method = 140
        keep_alive_timeout = 141
        session_taken_over = 142
        topic_filter_invalid = 143
        topic_name_invalid = 144
        packet_identifier_in_use = 145
        packet_identifier_not_found = 146
        receive_maximum_exceeded = 147
        topic_alias_invalid = 148
        packet_too_large = 149
        message_rate_too_high = 150
        quota_exceeded = 151
        administrative_action = 152
        payload_format_invalid = 153
        retain_not_supported = 154
        qos_not_supported = 155
        use_another_server = 156
        server_moved = 157
        shared_subscriptions_not_supported = 158
        connection_rate_exceeded = 159
        maximum_connect_time = 160
        subscription_identifiers_not_supported = 161
        wildcard_subscriptions_not_supported = 162
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.fixed_header = Mqtt.FixedHeader(self._io, self, self._root)
        self._raw_variable_header = self._io.read_bytes_full()
        _io__raw_variable_header = KaitaiStream(BytesIO(self._raw_variable_header))
        self.variable_header = Mqtt.VariableHeader(_io__raw_variable_header, self, self._root)

    class UserProperty(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name = Mqtt.MqttString(self._io, self, self._root)
            self.value = Mqtt.MqttString(self._io, self, self._root)


    class ConnectPayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.client_id = Mqtt.MqttString(self._io, self, self._root)
            if (self._parent._parent.variable_header.connect_flags & 4) != 0:
                self.will_properties = []
                for i in range(self._parent.properties_length.value):
                    self.will_properties.append(Mqtt.Property(self._io, self, self._root))


            if (self._parent._parent.variable_header.connect_flags & 4) != 0:
                self.will_topic = Mqtt.MqttString(self._io, self, self._root)

            if (self._parent._parent.variable_header.connect_flags & 4) != 0:
                self.will_payload = Mqtt.BinaryData(self._io, self, self._root)

            if (self._parent._parent.variable_header.connect_flags & 128) != 0:
                self.username = Mqtt.MqttString(self._io, self, self._root)

            if (self._parent._parent.variable_header.connect_flags & 64) != 0:
                self.password = Mqtt.MqttString(self._io, self, self._root)



    class FixedHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_type_and_flags = self._io.read_u1()
            self.remaining_length = Mqtt.VarInt(self._io, self, self._root)

        @property
        def packet_type(self):
            if hasattr(self, '_m_packet_type'):
                return self._m_packet_type

            self._m_packet_type = KaitaiStream.resolve_enum(Mqtt.PacketTypes, ((self.packet_type_and_flags & 240) >> 4))
            return getattr(self, '_m_packet_type', None)

        @property
        def dup_flag(self):
            if hasattr(self, '_m_dup_flag'):
                return self._m_dup_flag

            self._m_dup_flag = ((self.packet_type_and_flags & 8) >> 3)
            return getattr(self, '_m_dup_flag', None)

        @property
        def qos_level(self):
            if hasattr(self, '_m_qos_level'):
                return self._m_qos_level

            self._m_qos_level = ((self.packet_type_and_flags & 6) >> 1)
            return getattr(self, '_m_qos_level', None)

        @property
        def retain(self):
            if hasattr(self, '_m_retain'):
                return self._m_retain

            self._m_retain = (self.packet_type_and_flags & 1)
            return getattr(self, '_m_retain', None)


    class PublishPayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_bytes_full()


    class VarInt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte1 = self._io.read_u1()
            if (self.byte1 & 128) != 0:
                self.byte2 = self._io.read_u1()

            if (self.byte2 & 128) != 0:
                self.byte3 = self._io.read_u1()

            if (self.byte3 & 128) != 0:
                self.byte4 = self._io.read_u1()


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.byte1 & (((127 + (((self.byte2 & 127) << 7) * (1 if (self.byte1 & 128) != 0 else 0))) + (((self.byte3 & 127) << 14) * (1 if (self.byte2 & 128) != 0 else 0))) + (((self.byte4 & 127) << 21) * (1 if (self.byte3 & 128) != 0 else 0))))
            return getattr(self, '_m_value', None)


    class SubscribeTopic(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_filter = Mqtt.MqttString(self._io, self, self._root)
            self.subscription_options = self._io.read_u1()


    class SubscribePayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subscription_identifier = Mqtt.VarInt(self._io, self, self._root)
            self.topics = []
            i = 0
            while not self._io.is_eof():
                self.topics.append(Mqtt.SubscribeTopic(self._io, self, self._root))
                i += 1



    class VariableHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connect:
                self.protocol_name = Mqtt.MqttString(self._io, self, self._root)

            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connect:
                self.protocol_version = self._io.read_u1()

            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connect:
                self.connect_flags = self._io.read_u1()

            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connect:
                self.keep_alive = self._io.read_u2be()

            self.properties_length = Mqtt.VarInt(self._io, self, self._root)
            self.properties = []
            for i in range(self.properties_length.value):
                self.properties.append(Mqtt.Property(self._io, self, self._root))

            _on = self._parent.fixed_header.packet_type
            if _on == Mqtt.PacketTypes.connect:
                self.payload = Mqtt.ConnectPayload(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.publish:
                self.payload = Mqtt.PublishPayload(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.subscribe:
                self.payload = Mqtt.SubscribePayload(self._io, self, self._root)
            elif _on == Mqtt.PacketTypes.unsubscribe:
                self.payload = Mqtt.UnsubscribePayload(self._io, self, self._root)


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
            self.len_data = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.len_data)).decode(u"UTF-8")


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
                self.value = Mqtt.VarInt(self._io, self, self._root)
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


    class UnsubscribePayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topics = []
            i = 0
            while not self._io.is_eof():
                self.topics.append(Mqtt.MqttString(self._io, self, self._root))
                i += 1




