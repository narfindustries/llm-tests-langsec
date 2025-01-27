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
        self.input_count = self._io.read_u1()
        self.inputs = []
        for i in range(self.input_count):
            self.inputs.append(BitcoinTransaction.TxInput(self._io, self, self._root))

        self.output_count = self._io.read_u1()
        self.outputs = []
        for i in range(self.output_count):
            self.outputs.append(BitcoinTransaction.TxOutput(self._io, self, self._root))

        self.lock_time = self._io.read_u4le()

    class TxInput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.previous_output_hash = self._io.read_bytes(32)
            self.previous_output_index = self._io.read_u4le()
            self.script_length = self._io.read_u1()
            self.signature_script = self._io.read_bytes(self.script_length)
            self.sequence = self._io.read_u4le()


    class TxOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_length = self._io.read_u1()
            self.pk_script = self._io.read_bytes(self.script_length)



