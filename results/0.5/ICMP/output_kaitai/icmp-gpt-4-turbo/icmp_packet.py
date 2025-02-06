# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class IcmpPacket(KaitaiStruct):
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
            self.rest_of_header = IcmpPacket.IcmpTimestamp(self._io, self, self._root)
        elif _on == 17:
            self.rest_of_header = IcmpPacket.IcmpAddressMask(self._io, self, self._root)
        elif _on == 0:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == 13:
            self.rest_of_header = IcmpPacket.IcmpTimestamp(self._io, self, self._root)
        elif _on == 11:
            self.rest_of_header = IcmpPacket.IcmpTimeExceeded(self._io, self, self._root)
        elif _on == 12:
            self.rest_of_header = IcmpPacket.IcmpParameterProblem(self._io, self, self._root)
        elif _on == 3:
            self.rest_of_header = IcmpPacket.IcmpUnreach(self._io, self, self._root)
        elif _on == 5:
            self.rest_of_header = IcmpPacket.IcmpRedirect(self._io, self, self._root)
        elif _on == 8:
            self.rest_of_header = IcmpPacket.IcmpEcho(self._io, self, self._root)
        elif _on == 18:
            self.rest_of_header = IcmpPacket.IcmpAddressMask(self._io, self, self._root)
        else:
            self.rest_of_header = IcmpPacket.IcmpDefault(self._io, self, self._root)

    class IcmpParameterProblem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pointer = self._io.read_u1()
            self.unused = self._io.read_bits_int_be(3)
            self._io.align_to_byte()
            self.data = self._io.read_bytes_full()


    class IcmpRedirect(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gateway_internet_address = self._io.read_u4be()
            self.data = self._io.read_bytes_full()


    class IcmpDefault(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.data = self._io.read_bytes_full()


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


    class IcmpTimeExceeded(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.data = self._io.read_bytes_full()


    class IcmpUnreach(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.unused = self._io.read_u4be()
            self.data = self._io.read_bytes_full()


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



