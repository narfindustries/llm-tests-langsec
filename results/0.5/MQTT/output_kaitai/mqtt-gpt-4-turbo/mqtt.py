# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Mqtt(KaitaiStruct):
    """MQTT is a machine-to-machine (M2M)/"Internet of Things" connectivity protocol.
    It was designed as an extremely lightweight publish/subscribe messaging transport.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Mqtt.FixedHeader(self._io, self, self._root)
        _on = self.header.packet_type
        if _on == 14:
            self.body = Mqtt.DisconnectPacket(self._io, self, self._root)
        elif _on == 10:
            self.body = Mqtt.UnsubscribePacket(self._io, self, self._root)
        elif _on == 4:
            self.body = Mqtt.PubackPacket(self._io, self, self._root)
        elif _on == 6:
            self.body = Mqtt.PubrelPacket(self._io, self, self._root)
        elif _on == 7:
            self.body = Mqtt.PubcompPacket(self._io, self, self._root)
        elif _on == 1:
            self.body = Mqtt.ConnectPacket(self._io, self, self._root)
        elif _on == 13:
            self.body = Mqtt.EmptyPacket(self._io, self, self._root)
        elif _on == 11:
            self.body = Mqtt.UnsubackPacket(self._io, self, self._root)
        elif _on == 12:
            self.body = Mqtt.EmptyPacket(self._io, self, self._root)
        elif _on == 3:
            self.body = Mqtt.PublishPacket(self._io, self, self._root)
        elif _on == 5:
            self.body = Mqtt.PubrecPacket(self._io, self, self._root)
        elif _on == 15:
            self.body = Mqtt.AuthPacket(self._io, self, self._root)
        elif _on == 8:
            self.body = Mqtt.SubscribePacket(self._io, self, self._root)
        elif _on == 9:
            self.body = Mqtt.SubackPacket(self._io, self, self._root)
        elif _on == 2:
            self.body = Mqtt.ConnackPacket(self._io, self, self._root)

    class FixedHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_type_and_flags = self._io.read_u1()

        @property
        def packet_type(self):
            if hasattr(self, '_m_packet_type'):
                return self._m_packet_type

            self._m_packet_type = (self.packet_type_and_flags >> 4)
            return getattr(self, '_m_packet_type', None)

        @property
        def flags(self):
            if hasattr(self, '_m_flags'):
                return self._m_flags

            self._m_flags = (self.packet_type_and_flags & 15)
            return getattr(self, '_m_flags', None)


    class PubackPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class Subscription(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_filter = Mqtt.MqttString(self._io, self, self._root)
            self.options = self._io.read_u1()


    class ConnackPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ack_flags = self._io.read_u1()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class ConnectPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.protocol_name = Mqtt.MqttString(self._io, self, self._root)
            self.protocol_level = self._io.read_u1()
            self.connect_flags = self._io.read_u1()
            self.keep_alive = self._io.read_u2be()
            self.properties = Mqtt.Property(self._io, self, self._root)
            self.client_id = Mqtt.MqttString(self._io, self, self._root)
            if (self.connect_flags & 4) != 0:
                self.will_properties = Mqtt.Property(self._io, self, self._root)

            if (self.connect_flags & 4) != 0:
                self.will_topic = Mqtt.MqttString(self._io, self, self._root)

            if (self.connect_flags & 4) != 0:
                self.will_payload = Mqtt.MqttString(self._io, self, self._root)

            if (self.connect_flags & 128) != 0:
                self.username = Mqtt.MqttString(self._io, self, self._root)

            if (self.connect_flags & 64) != 0:
                self.password = Mqtt.MqttString(self._io, self, self._root)



    class PublishPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.topic_name = Mqtt.MqttString(self._io, self, self._root)
            if (self._parent.header.flags & 6) != 0:
                self.packet_id = self._io.read_u2be()

            self.properties = Mqtt.Property(self._io, self, self._root)
            self.payload = self._io.read_bytes_full()


    class PubcompPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class PubrelPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class EmptyPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class MqttString(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len = self._io.read_u2be()
            self.data = (self._io.read_bytes(self.len)).decode(u"UTF-8")


    class Property(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u1()
            self.value = self._io.read_u4be()


    class SubscribePacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.properties = Mqtt.Property(self._io, self, self._root)
            self.subscriptions = []
            i = 0
            while not self._io.is_eof():
                self.subscriptions.append(Mqtt.Subscription(self._io, self, self._root))
                i += 1



    class SubackPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.properties = Mqtt.Property(self._io, self, self._root)
            self.reason_codes = []
            i = 0
            while not self._io.is_eof():
                self.reason_codes.append(self._io.read_u1())
                i += 1



    class PubrecPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class UnsubscribePacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.properties = Mqtt.Property(self._io, self, self._root)
            self.topic_filters = []
            i = 0
            while not self._io.is_eof():
                self.topic_filters.append(Mqtt.MqttString(self._io, self, self._root))
                i += 1



    class AuthPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)


    class UnsubackPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.packet_id = self._io.read_u2be()
            self.properties = Mqtt.Property(self._io, self, self._root)
            self.reason_codes = []
            i = 0
            while not self._io.is_eof():
                self.reason_codes.append(self._io.read_u1())
                i += 1



    class DisconnectPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reason_code = self._io.read_u1()
            self.properties = Mqtt.Property(self._io, self, self._root)



