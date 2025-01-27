namespace HL7V2

structure HL7Message
{
    segments: Segment[]
}

structure Segment
{
    id: Bytes(3)
    content: BytesUntil('\r')
}

structure MSH
{
    segment_id: 'MSH'
    field_separator: Bytes(1)
    encoding_characters: Bytes(4)
    sending_application: VariableLengthField
    sending_facility: VariableLengthField
    receiving_application: VariableLengthField
    receiving_facility: VariableLengthField
    datetime_of_message: VariableLengthField
    security: VariableLengthField
    message_type: VariableLengthField
    message_control_id: VariableLengthField
    processing_id: VariableLengthField
    version_id: VariableLengthField
}

structure VariableLengthField
{
    length: UInt16
    value: Bytes(length)
}