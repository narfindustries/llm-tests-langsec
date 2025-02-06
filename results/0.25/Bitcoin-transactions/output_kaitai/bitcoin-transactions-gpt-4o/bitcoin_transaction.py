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
        for i in range(self.input_count.actual_value):
            self.inputs.append(BitcoinTransaction.Input(self._io, self, self._root))

        self.output_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.actual_value):
            self.outputs.append(BitcoinTransaction.Output(self._io, self, self._root))

        self.locktime = self._io.read_u4le()

    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.first_byte = self._io.read_u1()
            if self.first_byte == 253:
                self.value_2 = self._io.read_u2le()

            if self.first_byte == 254:
                self.value_4 = self._io.read_u4le()

            if self.first_byte == 255:
                self.value_8 = self._io.read_u8le()


        @property
        def actual_value(self):
            if hasattr(self, '_m_actual_value'):
                return self._m_actual_value

            self._m_actual_value = (self.value_2 if self.first_byte == 253 else (self.value_4 if self.first_byte == 254 else (self.value_8 if self.first_byte == 255 else self.first_byte)))
            return getattr(self, '_m_actual_value', None)


    class Input(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prev_tx_hash = self._io.read_bits_int_be(32)
            self._io.align_to_byte()
            self.output_index = self._io.read_u4le()
            self.script_sig_len = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_sig_len.actual_value)
            self.sequence = self._io.read_u4le()


    class Output(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_pubkey_len = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_pubkey = self._io.read_bytes(self.script_pubkey_len.actual_value)



