# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
    """The Internet Control Message Protocol (ICMP) is used by network devices,
    like routers, to send error messages and operational information indicating,
    for example, that a requested service is not available or that a host or
    router could not be reached.
    """

    class IcmpType(Enum):
        echo_reply = 0
        dest_unreachable = 3
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
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpInfo(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.timestamp_reply:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpTimestamp(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.info_request:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpInfo(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.dest_unreachable:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpDestUnreachable(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo_reply:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpEcho(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.source_quench:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpVoid(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.time_exceeded:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpVoid(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.echo_request:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpEcho(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.parameter_problem:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpParameterProblem(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.timestamp_request:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpTimestamp(_io__raw_rest_of_header, self, self._root)
        elif _on == IcmpPacket.IcmpType.redirect:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpRedirect(_io__raw_rest_of_header, self, self._root)
        else:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = IcmpPacket.IcmpVoid(_io__raw_rest_of_header, self, self._root)

    class IcmpParameterProblem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bytes(3)


    class IcmpRedirect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_internet_address = self._io.read_u4be()


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


    class IcmpVoid(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class IcmpDestUnreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()


    class IcmpInfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()


    class IcmpEcho(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()



