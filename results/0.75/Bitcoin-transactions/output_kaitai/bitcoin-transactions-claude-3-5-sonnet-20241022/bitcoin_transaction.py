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
        if self.version == 1:
            self.is_segwit = self._io.read_u1()

        self.input_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.final_value):
            self.inputs.append(BitcoinTransaction.TxInput(self._io, self, self._root))

        self.output_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.final_value):
            self.outputs.append(BitcoinTransaction.TxOutput(self._io, self, self._root))

        if self.is_segwit == 0:
            self.witness_data = BitcoinTransaction.Witness(self._io, self, self._root)

        self.locktime = self._io.read_u4le()

    class TxOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.script_pub_key = self._io.read_bytes(self.script_length.final_value)


    class Witness(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.count = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.items = []
            for i in range(self.count.final_value):
                self.items.append(BitcoinTransaction.WitnessItem(self._io, self, self._root))



    class VarInt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.flag = self._io.read_u1()
            if self.flag == 253:
                self.value_u2 = self._io.read_u2le()

            if self.flag == 254:
                self.value_u4 = self._io.read_u4le()

            if self.flag == 255:
                self.value_u8 = self._io.read_u8le()


        @property
        def final_value(self):
            if hasattr(self, '_m_final_value'):
                return self._m_final_value

            self._m_final_value = (self.flag if self.flag < 253 else (self.value_u2 if self.flag == 253 else (self.value_u4 if self.flag == 254 else self.value_u8)))
            return getattr(self, '_m_final_value', None)


    class WitnessItem(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.data = self._io.read_bytes(self.length.final_value)


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



