type Transaction = {
    version: u32,
    inputs: List<Input>,
    outputs: List<Output>,
    lockTime: u32
}

type Input = {
    previousTxHash: Bytes32,
    outputIndex: u32,
    scriptSig: Bytes,
    sequence: u32
}

type Output = {
    value: u64,
    scriptPubKey: Bytes
}

def parseTransaction(input: Bytes): Transaction = {
    let version = readU32(input, 0);
    let inputCount = readCompactSize(input, 4);
    let (inputs, inputsEndOffset) = parseInputs(input, 4 + compactSizeLength(inputCount));
    let outputCount = readCompactSize(input, inputsEndOffset);
    let (outputs, outputsEndOffset) = parseOutputs(input, inputsEndOffset + compactSizeLength(outputCount));
    let lockTime = readU32(input, outputsEndOffset);

    Transaction {
        version: version,
        inputs: inputs,
        outputs: outputs,
        lockTime: lockTime
    }
}

def parseInputs(input: Bytes, offset: usize): (List<Input>, usize) = {
    let inputCount = readCompactSize(input, offset);
    let mutable currentOffset = offset + compactSizeLength(inputCount);
    let mutable inputs = [];

    for _ in 0..inputCount {
        let previousTxHash = readBytes32(input, currentOffset);
        let outputIndex = readU32(input, currentOffset + 32);
        let (scriptSig, scriptSigLength) = readVarBytes(input, currentOffset + 36);
        let sequence = readU32(input, currentOffset + 36 + scriptSigLength + 1);

        inputs.push(Input {
            previousTxHash: previousTxHash,
            outputIndex: outputIndex,
            scriptSig: scriptSig,
            sequence: sequence
        });

        currentOffset += 36 + scriptSigLength + 1 + 4;
    }

    (inputs, currentOffset)
}

def parseOutputs(input: Bytes, offset: usize): (List<Output>, usize) = {
    let outputCount = readCompactSize(input, offset);
    let mutable currentOffset = offset + compactSizeLength(outputCount);
    let mutable outputs = [];

    for _ in 0..outputCount {
        let value = readU64(input, currentOffset);
        let (scriptPubKey, scriptPubKeyLength) = readVarBytes(input, currentOffset + 8);

        outputs.push(Output {
            value: value,
            scriptPubKey: scriptPubKey
        });

        currentOffset += 8 + scriptPubKeyLength + 1;
    }

    (outputs, currentOffset)
}

def readCompactSize(input: Bytes, offset: usize): u64 = {
    let firstByte = input[offset];
    match firstByte {
        0xFD => readU16(input, offset + 1),
        0xFE => readU32(input, offset + 1),
        0xFF => readU64(input, offset + 1),
        _ => firstByte as u64
    }
}

def compactSizeLength(value: u64): usize = {
    match value {
        x if x < 0xFD => 1,
        x if x <= 0xFFFF => 3,
        x if x <= 0xFFFFFFFF => 5,
        _ => 9
    }
}

def readVarBytes(input: Bytes, offset: usize): (Bytes, usize) = {
    let length = readCompactSize(input, offset);
    let bytesStart = offset + compactSizeLength(length);
    (input.slice(bytesStart, bytesStart + length as usize), length as usize + compactSizeLength(length))
}