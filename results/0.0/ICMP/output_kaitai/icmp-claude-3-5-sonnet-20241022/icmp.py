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
        self.type = KaitaiStream.resolve_enum(Icmp.MessageType, self._io.read_u1())
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        _on = self.type
        if _on == Icmp.MessageType.time_exceeded:
            self.rest_of_header = Icmp.UnusedData(self._io, self, self._root)
        elif _on == Icmp.MessageType.parameter_problem:
            self.rest_of_header = Icmp.ParameterData(self._io, self, self._root)
        elif _on == Icmp.MessageType.destination_unreachable:
            self.rest_of_header = Icmp.UnusedData(self._io, self, self._root)
        elif _on == Icmp.MessageType.information_request:
            self.rest_of_header = Icmp.InfoData(self._io, self, self._root)
        elif _on == Icmp.MessageType.source_quench:
            self.rest_of_header = Icmp.UnusedData(self._io, self, self._root)
        elif _on == Icmp.MessageType.redirect:
            self.rest_of_header = Icmp.GatewayAddress(self._io, self, self._root)
        elif _on == Icmp.MessageType.echo_reply:
            self.rest_of_header = Icmp.EchoData(self._io, self, self._root)
        elif _on == Icmp.MessageType.echo_request:
            self.rest_of_header = Icmp.EchoData(self._io, self, self._root)
        elif _on == Icmp.MessageType.timestamp:
            self.rest_of_header = Icmp.TimestampData(self._io, self, self._root)
        elif _on == Icmp.MessageType.timestamp_reply:
            self.rest_of_header = Icmp.TimestampData(self._io, self, self._root)
        elif _on == Icmp.MessageType.information_reply:
            self.rest_of_header = Icmp.InfoData(self._io, self, self._root)
        _on = self.type
        if _on == Icmp.MessageType.echo_reply:
            self.data = Icmp.ArbitraryData(self._io, self, self._root)
        elif _on == Icmp.MessageType.echo_request:
            self.data = Icmp.ArbitraryData(self._io, self, self._root)
        elif _on == Icmp.MessageType.timestamp:
            self.data = Icmp.TimestampPayload(self._io, self, self._root)
        elif _on == Icmp.MessageType.timestamp_reply:
            self.data = Icmp.TimestampPayload(self._io, self, self._root)
        else:
            self.data = Icmp.ErrorMessageData(self._io, self, self._root)

    class ErrorMessageData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.ip_header = self._io.read_bytes(20)
            self.original_datagram = self._io.read_bytes(8)


    class GatewayAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_ip = self._io.read_u4be()


    class EchoData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()


    class TimestampPayload(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.original_timestamp = self._io.read_u4be()
            self.receive_timestamp = self._io.read_u4be()
            self.transmit_timestamp = self._io.read_u4be()


    class ArbitraryData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.payload = self._io.read_bytes_full()


    class ParameterData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bytes(3)


    class UnusedData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()


    class InfoData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()


    class TimestampData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()



