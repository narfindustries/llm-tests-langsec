# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class BitcoinTransaction(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.version = self._io.read_u4le()
        self.input_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.value):
            self.inputs.append(BitcoinTransaction.TxInput(self._io, self, self._root))

        self.output_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.value):
            self.outputs.append(BitcoinTransaction.TxOutput(self._io, self, self._root))

        self.locktime = self._io.read_u4le()

    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.byte1 = self._io.read_u1()
            if self.byte1 == 255:
                self.byte2_8 = self._io.read_u8le()

            if self.byte1 == 254:
                self.byte2_4 = self._io.read_u4le()

            if self.byte1 == 253:
                self.byte2_2 = self._io.read_u2le()


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.byte2_8 if self.byte1 == 255 else (self.byte2_4 if self.byte1 == 254 else (self.byte2_2 if self.byte1 == 253 else self.byte1)))
            return getattr(self, '_m_value', None)


    class TxInput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.previous_output = BitcoinTransaction.Outpoint(self._io, self, self._root)
            self.script_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.signature_script = self._io.read_bytes(self.script_length.value)
            self.sequence = self._io.read_u4le()


    class TxOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.pubkey_script = self._io.read_bytes(self.script_length.value)


    class Outpoint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.hash = self._io.read_bytes(32)
            self.index = self._io.read_u4le()



