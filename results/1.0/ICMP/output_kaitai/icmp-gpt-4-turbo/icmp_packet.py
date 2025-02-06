# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
    """The Internet Control Message Protocol (ICMP) is used by network devices,
    like routers, to send error messages and operational information indicating
    issues such as communication failures and unreachable hosts. 
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = IcmpPacket.Header(self._io, self, self._root)
        _on = self.header.type
        if _on == 14:
            self.body = IcmpPacket.Timestamp(self._io, self, self._root)
        elif _on == 0:
            self.body = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == 4:
            self.body = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == 13:
            self.body = IcmpPacket.Timestamp(self._io, self, self._root)
        elif _on == 11:
            self.body = IcmpPacket.TimeExceeded(self._io, self, self._root)
        elif _on == 12:
            self.body = IcmpPacket.ParameterProblem(self._io, self, self._root)
        elif _on == 3:
            self.body = IcmpPacket.Unreachable(self._io, self, self._root)
        elif _on == 5:
            self.body = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == 15:
            self.body = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == 8:
            self.body = IcmpPacket.Echo(self._io, self, self._root)
        elif _on == 16:
            self.body = IcmpPacket.Echo(self._io, self, self._root)

    class Timestamp(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.originate_timestamp = self._io.read_u8be()
            self.receive_timestamp = self._io.read_u8be()
            self.transmit_timestamp = self._io.read_u8be()


    class TimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.internet_header_and_ip_payload = self._io.read_bytes_full()


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


    class Unreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            if self._parent.header.code == 4:
                self.next_hop_mtu = self._io.read_u2be()

            self.internet_header_and_ip_payload = self._io.read_bytes_full()


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u1()
            self.code = self._io.read_u1()
            self.checksum = self._io.read_u2be()


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
            self.internet_header_and_ip_payload = self._io.read_bytes_full()



