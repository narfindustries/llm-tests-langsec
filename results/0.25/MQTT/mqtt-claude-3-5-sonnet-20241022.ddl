def Main = MQTTPacket

def MQTTPacket = {
    fixed_header: FixedHeader
    variable_header: VariableHeader
    payload: Payload
}

def FixedHeader = {
    packet_type: uint4
    flags: PacketFlags
    remaining_length: VarByteInt
}

def PacketFlags = {
    dup: uint1
    qos: uint2
    retain: uint1
}

def VarByteInt = {
    byte1: uint8
    byte2: uint8
    byte3: uint8
    byte4: uint8
}

def VariableHeader = Select {
    ConnectHeader
    ConnackHeader
    PublishHeader
    PubackHeader
    PubrecHeader
    PubrelHeader
    PubcompHeader
    SubscribeHeader
    SubackHeader
    UnsubscribeHeader
    UnsubackHeader
    DisconnectHeader
    AuthHeader
}

def ConnectHeader = {
    protocol_name: MQTTString
    protocol_version: uint8
    connect_flags: ConnectFlags
    keep_alive: uint16
    properties: Properties
}

def ConnectFlags = {
    username_flag: uint1
    password_flag: uint1
    will_retain: uint1
    will_qos: uint2
    will_flag: uint1
    clean_start: uint1
    reserved: uint1
}

def ConnackHeader = {
    acknowledge_flags: uint8
    reason_code: uint8
    properties: Properties
}

def PublishHeader = {
    topic_name: MQTTString
    packet_id: uint16
    properties: Properties
}

def PubackHeader = {
    packet_id: uint16
    reason_code: uint8
    properties: Properties
}

def PubrecHeader = {
    packet_id: uint16
    reason_code: uint8
    properties: Properties
}

def PubrelHeader = {
    packet_id: uint16
    reason_code: uint8
    properties: Properties
}

def PubcompHeader = {
    packet_id: uint16
    reason_code: uint8
    properties: Properties
}

def SubscribeHeader = {
    packet_id: uint16
    properties: Properties
}

def SubackHeader = {
    packet_id: uint16
    properties: Properties
    reason_codes: ReasonCodes
}

def UnsubscribeHeader = {
    packet_id: uint16
    properties: Properties
}

def UnsubackHeader = {
    packet_id: uint16
    properties: Properties
    reason_codes: ReasonCodes
}

def DisconnectHeader = {
    reason_code: uint8
    properties: Properties
}

def AuthHeader = {
    reason_code: uint8
    properties: Properties
}

def Properties = {
    length: VarByteInt
    property_list: PropertyList
}

def PropertyList = {
    items: Property*
}

def Property = {
    identifier: uint8
    value: PropertyValue
}

def PropertyValue = Select {
    BooleanProperty
    ByteProperty
    TwoByteInteger
    FourByteInteger
    VariableByteInteger
    BinaryData
    UTF8String
    StringPair
}

def BooleanProperty = uint8
def ByteProperty = uint8
def TwoByteInteger = uint16
def FourByteInteger = uint32
def VariableByteInteger = VarByteInt
def BinaryData = MQTTBinary
def UTF8String = MQTTString
def StringPair = UserProperty

def UserProperty = {
    key: MQTTString
    value: MQTTString
}

def MQTTString = {
    length: uint16
    data: MQTTData
}

def MQTTData = {
    bytes: uint8*
}

def MQTTBinary = {
    length: uint16
    data: MQTTData
}

def ReasonCodes = {
    codes: uint8*
}

def Payload = Select {
    ConnectPayload
    PublishPayload
    SubscribePayload
    UnsubscribePayload
}

def ConnectPayload = {
    client_id: MQTTString
    will_properties: Properties
    will_topic: MQTTString
    will_payload: MQTTBinary
    username: MQTTString
    password: MQTTBinary
}

def PublishPayload = {
    data: MQTTData
}

def SubscribePayload = {
    topics: TopicFilterList
}

def TopicFilterList = {
    items: TopicFilter*
}

def TopicFilter = {
    filter: MQTTString
    options: uint8
}

def UnsubscribePayload = {
    topics: TopicList
}

def TopicList = {
    items: MQTTString*
}