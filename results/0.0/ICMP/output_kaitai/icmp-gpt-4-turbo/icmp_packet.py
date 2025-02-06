# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
    """The Internet Control Message Protocol (ICMP) is used by network devices to send error messages and operational information.
    """

    class IcmpType(Enum):
        echo_reply = 0
        destination_unreachable = 3
        source_quench = 4
        redirect = 5
        echo = 8
        time_exceeded = 11
        parameter_problem = 12
        timestamp = 13
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
        self.type = KaitaiStream.resolve_enum(IcmpPacket.IcmpType, self._io.read_u1())
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        _on = self.type
        if _on == IcmpPacket.IcmpType.timestamp_reply:
            self.rest_of_header = IcmpPacket.IcmpTimestamp(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.timestamp:
            self.rest_of_header = IcmpPacket.IcmpTimestamp(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo_reply:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.address_mask_request:
            self.rest_of_header = IcmpPacket.IcmpAddressMask(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.information_request:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.source_quench:
            self.rest_of_header = IcmpPacket.IcmpUnused(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.time_exceeded:
            self.rest_of_header = IcmpPacket.IcmpTimeExceeded(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.address_mask_reply:
            self.rest_of_header = IcmpPacket.IcmpAddressMask(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.parameter_problem:
            self.rest_of_header = IcmpPacket.IcmpParameterProblem(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.destination_unreachable:
            self.rest_of_header = IcmpPacket.IcmpDestinationUnreachable(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.information_reply:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.redirect:
            self.rest_of_header = IcmpPacket.IcmpRedirect(self._io, self, self._root)

    class IcmpParameterProblem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bits_int_be(24)
            self._io.align_to_byte()
            self.original_datagram = self._io.read_bytes_full()


    class IcmpRedirect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_internet_address = self._io.read_u4be()
            self.original_datagram = self._io.read_bytes_full()


    class IcmpTimestamp(KaitaiStruct):
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


    class IcmpUnused(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()


    class IcmpTimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.original_datagram = self._io.read_bytes_full()


    class IcmpDestinationUnreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.original_datagram = self._io.read_bytes_full()


    class IcmpAddressMask(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.address_mask = self._io.read_u4be()


    class IcmpEcho(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.data = self._io.read_bytes_full()



