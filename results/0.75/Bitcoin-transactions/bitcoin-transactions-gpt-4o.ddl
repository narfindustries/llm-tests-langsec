Define BitcoinTransaction 
As Record
    transaction_version    : UInt32
    input_count            : VarInt
    inputs                 : BitcoinInput[input_count]
    output_count           : VarInt
    outputs                : BitcoinOutput[output_count]
    lock_time              : UInt32
End Record

Define BitcoinInput 
As Record
    previous_transaction_hash : Bytes(32)
    output_index              : UInt32
    script_sig_length         : VarInt
    script_sig                : Bytes(script_sig_length)
    sequence                  : UInt32
End Record

Define BitcoinOutput 
As Record
    value               : UInt64
    script_pubkey_length: VarInt
    script_pubkey       : Bytes(script_pubkey_length)
End Record

Define VarInt 
As Choice
    UInt8 When $ < 0xFD
    UInt16 When $ == 0xFD
    UInt32 When $ == 0xFE
    UInt64 When $ == 0xFF
End Choice