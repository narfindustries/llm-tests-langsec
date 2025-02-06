# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Icmp(KaitaiStruct):

    class IcmpType(Enum):
        echo_reply = 0
        destination_unreachable = 3
        source_quench = 4
        redirect = 5
        echo_request = 8
        router_advertisement = 9
        router_solicitation = 10
        time_exceeded = 11
        parameter_problem = 12
        timestamp = 13
        timestamp_reply = 14
        information_request = 15
        information_reply = 16
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.type = KaitaiStream.resolve_enum(Icmp.IcmpType, self._io.read_u1())
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        _on = self.type
        if _on == Icmp.IcmpType.timestamp_reply:
            self.rest_of_header = Icmp.TimestampReply(self._io, self, self._root)
        elif _on == Icmp.IcmpType.timestamp:
            self.rest_of_header = Icmp.Timestamp(self._io, self, self._root)
        elif _on == Icmp.IcmpType.echo_reply:
            self.rest_of_header = Icmp.EchoMessage(self._io, self, self._root)
        elif _on == Icmp.IcmpType.time_exceeded:
            self.rest_of_header = Icmp.TimeExceeded(self._io, self, self._root)
        elif _on == Icmp.IcmpType.echo_request:
            self.rest_of_header = Icmp.EchoMessage(self._io, self, self._root)
        elif _on == Icmp.IcmpType.destination_unreachable:
            self.rest_of_header = Icmp.DestinationUnreachable(self._io, self, self._root)
        elif _on == Icmp.IcmpType.redirect:
            self.rest_of_header = Icmp.Redirect(self._io, self, self._root)
        else:
            self.rest_of_header = Icmp.RawRestOfHeader(self._io, self, self._root)

    class DestinationUnreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.original_datagram = []
            i = 0
            while not self._io.is_eof():
                self.original_datagram.append(self._io.read_bits_int_be(8))
                i += 1



    class Timestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.originate_timestamp = self._io.read_u4be()
            self.receive_timestamp = self._io.read_u4be()
            self.transmit_timestamp = self._io.read_u4be()


    class Redirect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_address = self._io.read_u4be()
            self.original_datagram = []
            i = 0
            while not self._io.is_eof():
                self.original_datagram.append(self._io.read_bits_int_be(8))
                i += 1



    class TimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.original_datagram = []
            i = 0
            while not self._io.is_eof():
                self.original_datagram.append(self._io.read_bits_int_be(8))
                i += 1



    class EchoMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.payload = []
            i = 0
            while not self._io.is_eof():
                self.payload.append(self._io.read_bits_int_be(8))
                i += 1



    class TimestampReply(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.originate_timestamp = self._io.read_u4be()
            self.receive_timestamp = self._io.read_u4be()
            self.transmit_timestamp = self._io.read_u4be()


    class RawRestOfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = self._io.read_u4be()



