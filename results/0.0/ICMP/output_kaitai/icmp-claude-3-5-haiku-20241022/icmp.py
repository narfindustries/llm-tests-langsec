# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Icmp(KaitaiStruct):

    class ParameterProblemCode(Enum):
        pointer_indicates_error = 0
        missing_required_option = 1
        bad_length = 2

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

    class RedirectCode(Enum):
        redirect_network = 0
        redirect_host = 1
        redirect_type_of_service_network = 2
        redirect_type_of_service_host = 3

    class TimeExceededCode(Enum):
        ttl_expired_in_transit = 0
        fragment_reassembly_time_exceeded = 1

    class DestinationUnreachableCode(Enum):
        net_unreachable = 0
        host_unreachable = 1
        protocol_unreachable = 2
        port_unreachable = 3
        fragmentation_needed = 4
        source_route_failed = 5
        destination_network_unknown = 6
        destination_host_unknown = 7
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
        elif _on == Icmp.IcmpType.router_advertisement:
            self.rest_of_header = Icmp.RouterAdvertisement(self._io, self, self._root)
        elif _on == Icmp.IcmpType.echo_reply:
            self.rest_of_header = Icmp.EchoMessage(self._io, self, self._root)
        elif _on == Icmp.IcmpType.time_exceeded:
            self.rest_of_header = Icmp.TimeExceeded(self._io, self, self._root)
        elif _on == Icmp.IcmpType.echo_request:
            self.rest_of_header = Icmp.EchoMessage(self._io, self, self._root)
        elif _on == Icmp.IcmpType.router_solicitation:
            self.rest_of_header = Icmp.RouterSolicitation(self._io, self, self._root)
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
            self._raw_original_datagram = self._io.read_bytes_full()
            _io__raw_original_datagram = KaitaiStream(BytesIO(self._raw_original_datagram))
            self.original_datagram = Icmp.ByteArray(_io__raw_original_datagram, self, self._root)


    class RouterSolicitation(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.reserved = self._io.read_u4be()


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
            self._raw_original_datagram = self._io.read_bytes_full()
            _io__raw_original_datagram = KaitaiStream(BytesIO(self._raw_original_datagram))
            self.original_datagram = Icmp.ByteArray(_io__raw_original_datagram, self, self._root)


    class RouterAddress(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.address = self._io.read_u4be()
            self.preference_level = self._io.read_u4be()


    class ByteArray(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = []
            i = 0
            while not self._io.is_eof():
                self.data.append(self._io.read_u1())
                i += 1



    class TimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self._raw_original_datagram = self._io.read_bytes_full()
            _io__raw_original_datagram = KaitaiStream(BytesIO(self._raw_original_datagram))
            self.original_datagram = Icmp.ByteArray(_io__raw_original_datagram, self, self._root)


    class RouterAdvertisement(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.number_of_addresses = self._io.read_u1()
            self.address_entry_size = self._io.read_u1()
            self.lifetime = self._io.read_u2be()
            self.router_addresses = []
            for i in range(self.number_of_addresses):
                self.router_addresses.append(Icmp.RouterAddress(self._io, self, self._root))



    class EchoMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self._raw_payload = self._io.read_bytes_full()
            _io__raw_payload = KaitaiStream(BytesIO(self._raw_payload))
            self.payload = Icmp.ByteArray(_io__raw_payload, self, self._root)


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



