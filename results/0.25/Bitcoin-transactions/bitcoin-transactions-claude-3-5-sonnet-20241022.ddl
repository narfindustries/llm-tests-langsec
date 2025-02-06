def Bitcoin = {
    Version: U32,
    InputCount: VarInt,
    Inputs: Input[InputCount],
    OutputCount: VarInt,
    Outputs: Output[OutputCount],
    LockTime: U32
}

def VarInt = {
    prefix: U8,
    value: U64
}

def Input = {
    PrevTxHash: U8[32],
    PrevTxIndex: U32,
    ScriptSigLength: VarInt,
    ScriptSig: U8[ScriptSigLength],
    Sequence: U32
}

def Output = {
    Value: U64,
    ScriptPubKeyLength: VarInt,
    ScriptPubKey: U8[ScriptPubKeyLength]
}

def WitnessTransaction = {
    Version: U32,
    Flag: U16 where Flag == 0x0001,
    InputCount: VarInt,
    Inputs: Input[InputCount],
    OutputCount: VarInt,
    Outputs: Output[OutputCount],
    WitnessData: WitnessField[InputCount],
    LockTime: U32
}

def WitnessField = {
    WitnessCount: VarInt,
    WitnessItems: WitnessItem[WitnessCount]
}

def WitnessItem = {
    Length: VarInt,
    Data: U8[Length]
}

entry Bitcoin | WitnessTransaction