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

    class ConnectHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.protocol_name_length = self._io.read_u2be()
            self.protocol_name = (self._io.read_bytes(self.protocol_name_length)).decode(u"utf8")
            self.protocol_version = self._io.read_u1()
            self.connect_flags = self._io.read_u1()
            self.keep_alive = self._io.read_u2be()

        @property
        def will_flag(self):
            if hasattr(self, '_m_will_flag'):
                return self._m_will_flag

            self._m_will_flag = ((self.connect_flags & 4) >> 2)
            return getattr(self, '_m_will_flag', None)

        @property
        def will_retain(self):
            if hasattr(self, '_m_will_retain'):
                return self._m_will_retain

            self._m_will_retain = ((self.connect_flags & 32) >> 5)
            return getattr(self, '_m_will_retain', None)

        @property
        def password_flag(self):
            if hasattr(self, '_m_password_flag'):
                return self._m_password_flag

            self._m_password_flag = ((self.connect_flags & 64) >> 6)
            return getattr(self, '_m_password_flag', None)

        @property
        def clean_start(self):
            if hasattr(self, '_m_clean_start'):
                return self._m_clean_start

            self._m_clean_start = ((self.connect_flags & 2) >> 1)
            return getattr(self, '_m_clean_start', None)

        @property
        def will_qos(self):
            if hasattr(self, '_m_will_qos'):
                return self._m_will_qos

            self._m_will_qos = ((self.connect_flags & 24) >> 3)
            return getattr(self, '_m_will_qos', None)

        @property
        def username_flag(self):
            if hasattr(self, '_m_username_flag'):
                return self._m_username_flag

            self._m_username_flag = ((self.connect_flags & 128) >> 7)
            return getattr(self, '_m_username_flag', None)


    class UserProperty(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.key = Mqtt.Utf8String(self._io, self, self._root)
            self.value = Mqtt.Utf8String(self._io, self, self._root)


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


    class Utf8String(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_value = self._io.read_u2be()
            self.value = (self._io.read_bytes(self.len_value)).decode(u"utf8")


    class ConnackHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.acknowledge_flags = self._io.read_u1()
            self.reason_code = self._io.read_u1()


    class PublishHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_name_length = self._io.read_u2be()
            self.topic_name = (self._io.read_bytes(self.topic_name_length)).decode(u"utf8")
            if self.qos > 0:
                self.packet_identifier = self._io.read_u2be()


        @property
        def dup(self):
            if hasattr(self, '_m_dup'):
                return self._m_dup

            self._m_dup = ((self._parent._parent.fixed_header.flags & 8) >> 3)
            return getattr(self, '_m_dup', None)

        @property
        def qos(self):
            if hasattr(self, '_m_qos'):
                return self._m_qos

            self._m_qos = ((self._parent._parent.fixed_header.flags & 6) >> 1)
            return getattr(self, '_m_qos', None)

        @property
        def retain(self):
            if hasattr(self, '_m_retain'):
                return self._m_retain

            self._m_retain = (self._parent._parent.fixed_header.flags & 1)
            return getattr(self, '_m_retain', None)


    class Properties(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = Mqtt.Vlq(self._io, self, self._root)
            self.property_list = []
            i = 0
            while not self._io.is_eof():
                self.property_list.append(Mqtt.Property(self._io, self, self._root))
                i += 1



    class VariableHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connect:
                self.connect_header = Mqtt.ConnectHeader(self._io, self, self._root)

            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.connack:
                self.connack_header = Mqtt.ConnackHeader(self._io, self, self._root)

            if self._parent.fixed_header.packet_type == Mqtt.PacketTypes.publish:
                self.publish_header = Mqtt.PublishHeader(self._io, self, self._root)

            self.properties = Mqtt.Properties(self._io, self, self._root)
            self._raw_payload = self._io.read_bytes_full()
            _io__raw_payload = KaitaiStream(BytesIO(self._raw_payload))
            self.payload = Mqtt.Payload(_io__raw_payload, self, self._root)


    class BinaryData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_value = self._io.read_u2be()
            self.value = self._io.read_bytes(self.len_value)


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
                self.value = Mqtt.Utf8String(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.subscription_identifier_available:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.server_reference:
                self.value = Mqtt.Utf8String(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.response_information:
                self.value = Mqtt.Utf8String(self._io, self, self._root)
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
                self.value = Mqtt.Utf8String(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.assigned_client_identifier:
                self.value = Mqtt.Utf8String(self._io, self, self._root)
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
                self.value = Mqtt.Utf8String(self._io, self, self._root)
            elif _on == Mqtt.PropertyIds.request_response_information:
                self.value = self._io.read_u1()
            elif _on == Mqtt.PropertyIds.response_topic:
                self.value = Mqtt.Utf8String(self._io, self, self._root)


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

            self._m_value = ((((self.groups[0] & 127) + (((self.groups[1] & 127) << 7) if len(self.groups) >= 2 else 0)) + (((self.groups[2] & 127) << 14) if len(self.groups) >= 3 else 0)) + (((self.groups[3] & 127) << 21) if len(self.groups) >= 4 else 0))
            return getattr(self, '_m_value', None)



