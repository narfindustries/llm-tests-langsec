# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Icmp(KaitaiStruct):

    class MessageType(Enum):
        echo_reply = 0
        destination_unreachable = 3
        source_quench = 4
        redirect = 5
        echo_request = 8
        time_exceeded = 11
        parameter_problem = 12
        timestamp_request = 13
        timestamp_reply = 14
        information_request = 15
        information_reply = 16
        address_mask_request = 17
        address_mask_reply = 18
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.type = KaitaiStream.resolve_enum(Icmp.MessageType, self._io.read_u1())
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        self.rest_of_header = self._io.read_u4be()
        self.payload = self._io.read_bytes_full()

    @property
    def receive_timestamp(self):
        if hasattr(self, '_m_receive_timestamp'):
            return self._m_receive_timestamp

        if  ((self.type == Icmp.MessageType.timestamp_request) or (self.type == Icmp.MessageType.timestamp_reply)) :
            _pos = self._io.pos()
            self._io.seek(8)
            self._m_receive_timestamp = self._io.read_u4be()
            self._io.seek(_pos)

        return getattr(self, '_m_receive_timestamp', None)

    @property
    def originate_timestamp(self):
        if hasattr(self, '_m_originate_timestamp'):
            return self._m_originate_timestamp

        if  ((self.type == Icmp.MessageType.timestamp_request) or (self.type == Icmp.MessageType.timestamp_reply)) :
            _pos = self._io.pos()
            self._io.seek(4)
            self._m_originate_timestamp = self._io.read_u4be()
            self._io.seek(_pos)

        return getattr(self, '_m_originate_timestamp', None)

    @property
    def gateway_address(self):
        if hasattr(self, '_m_gateway_address'):
            return self._m_gateway_address

        if self.type == Icmp.MessageType.redirect:
            _pos = self._io.pos()
            self._io.seek(4)
            self._m_gateway_address = self._io.read_u4be()
            self._io.seek(_pos)

        return getattr(self, '_m_gateway_address', None)

    @property
    def unused(self):
        if hasattr(self, '_m_unused'):
            return self._m_unused

        if  ((self.type == Icmp.MessageType.destination_unreachable) or (self.type == Icmp.MessageType.time_exceeded) or (self.type == Icmp.MessageType.parameter_problem)) :
            _pos = self._io.pos()
            self._io.seek(4)
            self._m_unused = self._io.read_u4be()
            self._io.seek(_pos)

        return getattr(self, '_m_unused', None)

    @property
    def transmit_timestamp(self):
        if hasattr(self, '_m_transmit_timestamp'):
            return self._m_transmit_timestamp

        if  ((self.type == Icmp.MessageType.timestamp_request) or (self.type == Icmp.MessageType.timestamp_reply)) :
            _pos = self._io.pos()
            self._io.seek(12)
            self._m_transmit_timestamp = self._io.read_u4be()
            self._io.seek(_pos)

        return getattr(self, '_m_transmit_timestamp', None)

    @property
    def sequence_number(self):
        if hasattr(self, '_m_sequence_number'):
            return self._m_sequence_number

        if  ((self.type == Icmp.MessageType.echo_reply) or (self.type == Icmp.MessageType.echo_request) or (self.type == Icmp.MessageType.timestamp_request) or (self.type == Icmp.MessageType.timestamp_reply) or (self.type == Icmp.MessageType.information_request) or (self.type == Icmp.MessageType.information_reply)) :
            _pos = self._io.pos()
            self._io.seek(6)
            self._m_sequence_number = self._io.read_u2be()
            self._io.seek(_pos)

        return getattr(self, '_m_sequence_number', None)

    @property
    def pointer(self):
        if hasattr(self, '_m_pointer'):
            return self._m_pointer

        if self.type == Icmp.MessageType.parameter_problem:
            _pos = self._io.pos()
            self._io.seek(4)
            self._m_pointer = self._io.read_u1()
            self._io.seek(_pos)

        return getattr(self, '_m_pointer', None)

    @property
    def identifier(self):
        if hasattr(self, '_m_identifier'):
            return self._m_identifier

        if  ((self.type == Icmp.MessageType.echo_reply) or (self.type == Icmp.MessageType.echo_request) or (self.type == Icmp.MessageType.timestamp_request) or (self.type == Icmp.MessageType.timestamp_reply) or (self.type == Icmp.MessageType.information_request) or (self.type == Icmp.MessageType.information_reply)) :
            _pos = self._io.pos()
            self._io.seek(4)
            self._m_identifier = self._io.read_u2be()
            self._io.seek(_pos)

        return getattr(self, '_m_identifier', None)


