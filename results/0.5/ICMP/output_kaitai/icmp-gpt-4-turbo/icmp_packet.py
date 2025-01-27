# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
    """The Internet Control Message Protocol (ICMP) is used by network devices,
    like routers, to send error messages and operational information.
    """

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
        timestamp_request = 13
        timestamp_reply = 14
        info_request = 15
        info_reply = 16
        address_mask_request = 17
        address_mask_reply = 18
        traceroute = 30
        datagram_conversion_error = 31
        mobile_host_redirect = 32
        ipv6_where_are_you = 33
        ipv6_i_am_here = 34
        mobile_registration_request = 35
        mobile_registration_reply = 36
        domain_name_request = 37
        domain_name_reply = 38
        skip = 39
        photuris = 40
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
        if _on == IcmpPacket.IcmpType.info_reply:
            self.rest_of_header = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.timestamp_reply:
            self.rest_of_header = IcmpPacket.Timestamp(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.info_request:
            self.rest_of_header = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo_reply:
            self.rest_of_header = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.address_mask_request:
            self.rest_of_header = IcmpPacket.AddressMask(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.source_quench:
            self.rest_of_header = IcmpPacket.Empty(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.time_exceeded:
            self.rest_of_header = IcmpPacket.Empty(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo_request:
            self.rest_of_header = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.address_mask_reply:
            self.rest_of_header = IcmpPacket.AddressMask(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.parameter_problem:
            self.rest_of_header = IcmpPacket.ParameterProblem(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.timestamp_request:
            self.rest_of_header = IcmpPacket.Timestamp(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.destination_unreachable:
            self.rest_of_header = IcmpPacket.DestinationUnreachable(self._io, self, self._root)
        elif _on == IcmpPacket.IcmpType.redirect:
            self.rest_of_header = IcmpPacket.Redirect(self._io, self, self._root)

    class DestinationUnreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.next_hop_mtu = self._io.read_u2be()
            self.data = self._io.read_bytes_full()


    class Empty(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


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
            self.gateway_internet_address = self._io.read_u4be()
            self.data = self._io.read_bytes_full()


    class AddressMask(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.address_mask = self._io.read_u4be()


    class Echo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.data = self._io.read_bytes_full()


    class ParameterProblem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bits_int_be(24)
            self._io.align_to_byte()
            self.data = self._io.read_bytes_full()



