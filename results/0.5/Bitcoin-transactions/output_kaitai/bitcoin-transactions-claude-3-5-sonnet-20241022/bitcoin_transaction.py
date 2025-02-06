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
        if self.version >= 2:
            self.is_segwit = BitcoinTransaction.SegwitMarker(self._io, self, self._root)

        self.input_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.final_value):
            self.inputs.append(BitcoinTransaction.TxInput(self._io, self, self._root))

        self.output_count = BitcoinTransaction.VarInt(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.final_value):
            self.outputs.append(BitcoinTransaction.TxOutput(self._io, self, self._root))

        if self.is_segwit.is_segwit:
            self.witnesses = []
            for i in range(self.input_count.final_value):
                self.witnesses.append(BitcoinTransaction.WitnessData(self._io, self, self._root))


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


    class VarInt(KaitaiStruct):
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
            self.previous_output_hash = self._io.read_bytes(32)
            self.previous_output_index = self._io.read_u4le()
            self.script_length = BitcoinTransaction.VarInt(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_length.final_value)
            self.sequence = self._io.read_u4le()


    class SegwitMarker(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.marker = self._io.read_u1()
            self.flag = self._io.read_u1()

        @property
        def is_segwit(self):
            if hasattr(self, '_m_is_segwit'):
                return self._m_is_segwit

            self._m_is_segwit =  ((self.marker == 0) and (self.flag == 1)) 
            return getattr(self, '_m_is_segwit', None)



