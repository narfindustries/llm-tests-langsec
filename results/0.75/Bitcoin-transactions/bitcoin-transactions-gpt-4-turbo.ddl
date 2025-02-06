module Bitcoin.Transaction;

import Binary; // Import necessary module for binary handling

type VarInt = struct {
    value: u8 varint;
};

type TxIn = struct {
    prevTx     : bytes[32]; // Previous transaction hash
    prevIndex  : u32;       // Output index in the previous transaction
    scriptLen  : VarInt;    // Length of the scriptSig, defined as a variable integer
    scriptSig  : bytes[scriptLen.value]; // ScriptSig
    sequence   : u32;       // Sequence number
};

type TxOut = struct {
    value       : u64;              // Amount in satoshis
    scriptLen   : VarInt;           // Length of the ScriptPubKey, defined as a variable integer
    scriptPubKey: bytes[scriptLen.value]; // ScriptPubKey
};

type Witness = struct {
    numWitnessElements: VarInt;     // Number of witness elements
    witnessData: sequence of struct { // Dynamic sequence of witness data
        witnessDataLength : VarInt; // Length of each witness element
        witnessData       : bytes[witnessDataLength.value]; // Witness data itself
    } [numWitnessElements.value];
};

type Transaction = struct {
    version     : u32;          // Version of the transaction
    inCount     : VarInt;       // Number of transaction inputs
    inputs      : TxIn[inCount.value]; // Transaction inputs
    outCount    : VarInt;       // Number of transaction outputs
    outputs     : TxOut[outCount.value]; // Transaction outputs
    witnessFlag : u8;           // Flag for witness data presence (0x00 for no witness, 0x01 if present)
    witness     : Witness[1] if witnessFlag == 0x01; // Witnesses for each input, present if witnessFlag is 0x01
    locktime    : u32;          // Transaction lock time
};