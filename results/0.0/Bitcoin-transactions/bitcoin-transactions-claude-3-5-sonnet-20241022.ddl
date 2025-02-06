def Bitcoin = {
    Version: U32,
    InputCount: VarInt,
    Inputs: Input[InputCount],
    OutputCount: VarInt,
    Outputs: Output[OutputCount],
    WitnessData: WitnessSection,
    LockTime: U32
}

def WitnessSection = {
    HasWitness: U8,
    Data: if HasWitness == 0x00 then {
        Flag: U8 where Flag == 0x01,
        WitnessItems: WitnessItem[parent.InputCount]
    }
}

def VarInt = {
    FirstByte: U8,
    Value: if FirstByte < 0xFD then FirstByte
           else if FirstByte == 0xFD then U16
           else if FirstByte == 0xFE then U32
           else U64
}

def Input = {
    PrevTxHash: U8[32],
    PrevTxIndex: U32,
    ScriptLength: VarInt,
    ScriptSig: U8[ScriptLength],
    Sequence: U32
}

def Output = {
    Value: U64,
    ScriptLength: VarInt,
    ScriptPubKey: U8[ScriptLength]
}

def WitnessItem = {
    WitnessCount: VarInt,
    WitnessData: {
        Length: VarInt,
        Data: U8[Length]
    }[WitnessCount]
}