# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Icmp(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.type = self._io.read_u1()
        self.code = self._io.read_u1()
        self.checksum = self._io.read_u2be()
        _on = self.type
        if _on == 14:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.TimestampMessage(_io__raw_rest_of_header, self, self._root)
        elif _on == 17:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.AddressMaskMessage(_io__raw_rest_of_header, self, self._root)
        elif _on == 0:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.EchoMessage(_io__raw_rest_of_header, self, self._root)
        elif _on == 4:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.Unused(_io__raw_rest_of_header, self, self._root)
        elif _on == 13:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.TimestampMessage(_io__raw_rest_of_header, self, self._root)
        elif _on == 11:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.TimeExceeded(_io__raw_rest_of_header, self, self._root)
        elif _on == 12:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.ParameterProblem(_io__raw_rest_of_header, self, self._root)
        elif _on == 3:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.DestinationUnreachable(_io__raw_rest_of_header, self, self._root)
        elif _on == 5:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.Redirect(_io__raw_rest_of_header, self, self._root)
        elif _on == 15:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.Unused(_io__raw_rest_of_header, self, self._root)
        elif _on == 8:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.EchoMessage(_io__raw_rest_of_header, self, self._root)
        elif _on == 16:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.Unused(_io__raw_rest_of_header, self, self._root)
        elif _on == 18:
            self._raw_rest_of_header = self._io.read_bytes(4)
            _io__raw_rest_of_header = KaitaiStream(BytesIO(self._raw_rest_of_header))
            self.rest_of_header = Icmp.AddressMaskMessage(_io__raw_rest_of_header, self, self._root)
        else:
            self.rest_of_header = self._io.read_bytes(4)

    class DestinationUnreachable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u2be()
            self.mtu = self._io.read_u2be()


    class Redirect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_internet_address = Icmp.Ipv4Address(self._io, self, self._root)


    class TimestampMessage(KaitaiStruct):
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


    class Unused(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()


    class Ipv4Address(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.b1 = self._io.read_u1()
            self.b2 = self._io.read_u1()
            self.b3 = self._io.read_u1()
            self.b4 = self._io.read_u1()

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((((self.b1 << 24) | (self.b2 << 16)) | (self.b3 << 8)) | self.b4)
            return getattr(self, '_m_value', None)


    class TimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()


    class EchoMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()


    class AddressMaskMessage(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.identifier = self._io.read_u2be()
            self.sequence_number = self._io.read_u2be()
            self.address_mask = self._io.read_u4be()


    class ParameterProblem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bytes(3)



