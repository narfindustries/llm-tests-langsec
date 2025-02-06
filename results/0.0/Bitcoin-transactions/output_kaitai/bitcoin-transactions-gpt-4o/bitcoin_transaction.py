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
            self.value = self._io.read_u1()
            if self.value == 253:
                self.value_16 = self._io.read_u2le()

            if self.value == 254:
                self.value_32 = self._io.read_u4le()

            if self.value == 255:
                self.value_64 = self._io.read_u8le()


        @property
        def actual_value(self):
            if hasattr(self, '_m_actual_value'):
                return self._m_actual_value

            self._m_actual_value = (self.value_16 if self.value == 253 else (self.value_32 if self.value == 254 else (self.value_64 if self.value == 255 else self.value)))
            return getattr(self, '_m_actual_value', None)


    class Input(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prev_tx_hash = (self._io.read_bytes(32)).decode(u"ascii")
            self.output_index = self._io.read_u4le()
            self.script_sig_len = BitcoinTransaction.Varint(self._io, self, self._root)
            self.script_sig = (self._io.read_bytes(self.script_sig_len.actual_value)).decode(u"ascii")
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
            self.script_pubkey = (self._io.read_bytes(self.script_pubkey_len.actual_value)).decode(u"ascii")



