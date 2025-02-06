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
        self.num_inputs = self._io.read_u4le()
        self.inputs = []
        for i in range(self.num_inputs):
            self.inputs.append(BitcoinTransaction.TransactionInput(self._io, self, self._root))

        self.num_outputs = self._io.read_u4le()
        self.outputs = []
        for i in range(self.num_outputs):
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
            self.prev_tx_output_index = self._io.read_u4le()
            self.len_script_sig = self._io.read_u4le()
            self.script_sig = self._io.read_bytes(self.len_script_sig)
            self.sequence_number = self._io.read_u4le()


    class TransactionOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.amount = self._io.read_u8le()
            self.len_script_pubkey = self._io.read_u4le()
            self.script_pubkey = self._io.read_bytes(self.len_script_pubkey)



