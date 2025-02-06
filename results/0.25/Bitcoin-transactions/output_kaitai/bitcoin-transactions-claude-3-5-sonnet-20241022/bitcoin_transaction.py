# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class BitcoinTransaction(KaitaiStruct):

    class SighashType(Enum):
        all = 1
        none = 2
        single = 3
        anyone_can_pay = 128
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.version = self._io.read_u4le()
        if self.version >= 2:
            self.is_segwit = self._io.read_u1()

        if self.is_segwit == 1:
            self.flag = self._io.read_u1()

        self.input_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.final_value):
            self.inputs.append(BitcoinTransaction.TxIn(self._io, self, self._root))

        self.output_count = BitcoinTransaction.Varint(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.final_value):
            self.outputs.append(BitcoinTransaction.TxOut(self._io, self, self._root))

        if self.is_segwit == 1:
            self.witness_data = []
            for i in range(self.input_count.final_value):
                self.witness_data.append(BitcoinTransaction.Witness(self._io, self, self._root))


        self.lock_time = self._io.read_u4le()

    class TxIn(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.previous_tx_hash = self._io.read_bytes(32)
            self.previous_tx_index = self._io.read_u4le()
            self.script_sig_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_sig_length.final_value)
            self.sequence = self._io.read_u4le()


    class Witness(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.witness_count = BitcoinTransaction.Varint(self._io, self, self._root)
            self.items = []
            for i in range(self.witness_count.final_value):
                self.items.append(BitcoinTransaction.WitnessItem(self._io, self, self._root))



    class TxOut(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_pubkey_length = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_pubkey = self._io.read_bytes(self.script_pubkey_length.final_value)


    class Varint(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prefix = self._io.read_u1()
            if self.prefix == 253:
                self.value_u2 = self._io.read_u2le()

            if self.prefix == 254:
                self.value_u4 = self._io.read_u4le()

            if self.prefix == 255:
                self.value_u8 = self._io.read_u8le()


        @property
        def final_value(self):
            if hasattr(self, '_m_final_value'):
                return self._m_final_value

            self._m_final_value = (self.prefix if self.prefix < 253 else (self.value_u2 if self.prefix == 253 else (self.value_u4 if self.prefix == 254 else self.value_u8)))
            return getattr(self, '_m_final_value', None)


    class WitnessItem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size = BitcoinTransaction.Varint(self._io, self, self._root)
            self.data = self._io.read_bytes(self.size.final_value)



