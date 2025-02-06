# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class BitcoinTransaction(KaitaiStruct):

    class TxVersion(Enum):
        legacy = 1
        segwit = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.version = self._io.read_u4le()
        if self.version >= 2:
            self.is_segwit = self._io.read_bits_int_be(1) != 0

        self._io.align_to_byte()
        self.input_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.final_value):
            self.inputs.append(BitcoinTransaction.TxInput(self._io, self, self._root))

        self.output_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.final_value):
            self.outputs.append(BitcoinTransaction.TxOutput(self._io, self, self._root))

        if self.is_segwit == True:
            self.witnesses = []
            for i in range(self.input_count.final_value):
                self.witnesses.append(BitcoinTransaction.WitnessData(self._io, self, self._root))


        self.lock_time = self._io.read_u4le()

    class TxOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.script_pubkey = self._io.read_bytes(self.script_length.final_value)


    class VarInt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prefix = self._io.read_u1()
            _on = self.prefix
            if _on == 253:
                self.raw_value = self._io.read_u2le()
            elif _on == 254:
                self.raw_value = self._io.read_u4le()
            elif _on == 255:
                self.raw_value = self._io.read_u8le()

        @property
        def final_value(self):
            if hasattr(self, '_m_final_value'):
                return self._m_final_value

            self._m_final_value = (self.prefix if self.prefix <= 252 else self.raw_value)
            return getattr(self, '_m_final_value', None)


    class WitnessItem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.item_length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.witness_data = self._io.read_bytes(self.item_length.final_value)


    class WitnessData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.witness_count = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.witness_items = []
            for i in range(self.witness_count.final_value):
                self.witness_items.append(BitcoinTransaction.WitnessItem(self._io, self, self._root))



    class TxInput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.previous_tx_hash = self._io.read_bytes(32)
            self.previous_output_index = self._io.read_u4le()
            self.script_length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_length.final_value)
            self.sequence = self._io.read_u4le()



