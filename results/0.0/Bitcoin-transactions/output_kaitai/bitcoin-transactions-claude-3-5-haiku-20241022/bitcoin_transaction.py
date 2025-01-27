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
        self.tx_in_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.inputs = []
        for i in range(self.tx_in_count.value):
            self.inputs.append(BitcoinTransaction.TransactionInput(self._io, self, self._root))

        self.tx_out_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.outputs = []
        for i in range(self.tx_out_count.value):
            self.outputs.append(BitcoinTransaction.TransactionOutput(self._io, self, self._root))

        self.locktime = self._io.read_u4le()

    class TransactionInput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prev_tx_hash = self._io.read_bytes(32)
            self.prev_tx_index = self._io.read_u4le()
            self.script_sig_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_sig_length.value)
            self.sequence = self._io.read_u4le()


    class TransactionOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_pub_key_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_pub_key = self._io.read_bytes(self.script_pub_key_length.value)


    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            if self.value < 253:
                self.single_byte = self._io.read_u1()

            if self.value == 253:
                self.two_bytes = self._io.read_u2le()

            if self.value == 254:
                self.four_bytes = self._io.read_u4le()

            if self.value == 255:
                self.eight_bytes = self._io.read_u8le()


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = (self.single_byte if self.single_byte != 0 else (self.two_bytes if self.two_bytes != 0 else (self.four_bytes if self.four_bytes != 0 else self.eight_bytes)))
            return getattr(self, '_m_value', None)



