type Transaction = {
    inputs: List<Input>,
    outputs: List<Output>,
    lockTime: U32
}

type Input = {
    txHash: Bytes32,
    outputIndex: U32,
    scriptSig: Bytes,
    sequence: U32
}

type Output = {
    value: U64,
    scriptPubKey: Bytes
}

def validateTransaction(tx: Transaction) -> Bool {
    validateInputs(tx.inputs) && 
    validateOutputs(tx.outputs) && 
    calculateTotalInputValue(tx.inputs) >= calculateTotalOutputValue(tx.outputs)
}

def validateInputs(inputs: List<Input>) -> Bool {
    inputs.length > 0 && 
    inputs.all(input -> 
        input.txHash != Bytes32.zero() && 
        input.scriptSig.length > 0
    )
}

def validateOutputs(outputs: List<Output>) -> Bool {
    outputs.length > 0 && 
    outputs.all(output -> output.value > 0)
}

def calculateTotalInputValue(inputs: List<Input>) -> U64 {
    inputs.fold(0, (acc, input) -> acc + lookupInputValue(input))
}

def calculateTotalOutputValue(outputs: List<Output>) -> U64 {
    outputs.fold(0, (acc, output) -> acc + output.value)
}

def lookupInputValue(input: Input) -> U64 {
    // Placeholder for actual input value lookup mechanism
    0
}

def parseTransaction(rawTx: Bytes) -> Transaction {
    // Implement transaction parsing logic
    Transaction {
        inputs: [],
        outputs: [],
        lockTime: 0
    }
}

def serializeTransaction(tx: Transaction) -> Bytes {
    // Implement transaction serialization logic
    Bytes.empty()
}