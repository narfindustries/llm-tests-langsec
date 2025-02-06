module MQTTv5

import Data.Bits
import Data.Word
import Data.Maybe
import Data.Text (Text)
import Data.ByteString (ByteString, pack)
import Data.List (unfoldr)

-- Fixed Header
data FixedHeader = FixedHeader {
    messageType :: Word8,
    remainingLength :: Word32,
    flags :: Word8
} deriving (Show, Eq)

-- Variable Header Properties (Partial - Add all properties from MQTT v5.0 spec)
data Property =
    PayloadFormatIndicator Word8
  | ContentType Text
  | ResponseTopic Text
  | CorrelationData ByteString
  | UserProperty (Text, Text)
  | SubscriptionIdentifier Word32
  | MessageExpiryInterval Word32
  | TopicAlias Word32
  | ReasonString Text
  | ServerKeepAlive Word16
  | AssignedClientIdentifier Text
  | AuthenticationMethod Text
  | AuthenticationData ByteString
  | RequestProblemInformation Bool
  | WillDelayInterval Word32
  | MaximumPacketSize Word32
  | ReceiveMaximum Word16
  | TopicAliasMaximum Word16
  | MaximumQos Word8
  | RetainAvailable Bool
  | WildcardSubscriptionAvailable Bool
  | SubscriptionIdentifierAvailable Bool
  | SharedSubscriptionAvailable Bool
  | SessionExpiryInterval Word32
  | SessionTakeOver Bool
  | ServerReference Text
  | -- Add other property types here ...
  UnknownProperty ByteString  -- for handling unknown properties
  deriving (Show, Eq)

data Properties = Properties {
    propertiesList :: [Property]
} deriving (Show, Eq)


-- Specific Message Types (Examples)

-- CONNECT
data ConnectMessage = ConnectMessage {
    fixedHeader :: FixedHeader,
    protocolName :: Text,
    protocolLevel :: Word8,
    connectFlags :: Word8,
    keepAlive :: Word16,
    clientIdentifier :: Text,
    willMessage :: Maybe WillMessage,
    userName :: Maybe Text,
    password :: Maybe ByteString,
    properties :: Properties
} deriving (Show, Eq)

data WillMessage = WillMessage {
    willTopic :: Text,
    willQoS :: Word8,
    willRetain :: Bool,
    willPayload :: ByteString,
    willProperties :: Properties
} deriving (Show, Eq)


-- CONNACK
data ConnackMessage = ConnackMessage {
    fixedHeader :: FixedHeader,
    sessionPresent :: Bool,
    connectReturnCode :: Word8,
    properties :: Properties
} deriving (Show, Eq)

-- PUBLISH
data PublishMessage = PublishMessage {
    fixedHeader :: FixedHeader,
    topicName :: Text,
    packetId :: Maybe Word16,
    payload :: ByteString,
    properties :: Properties
} deriving (Show, Eq)

-- PUBACK
data PubackMessage = PubackMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    properties :: Properties
} deriving (Show,Eq)

-- PUBREC
data PubrecMessage = PubrecMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    properties :: Properties
} deriving (Show,Eq)

-- PUBREL
data PubrelMessage = PubrelMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    properties :: Properties
} deriving (Show,Eq)

-- PUBCOMP
data PubcompMessage = PubcompMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    properties :: Properties
} deriving (Show,Eq)

-- SUBSCRIBE
data SubscribeMessage = SubscribeMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    subscriptions :: [(Text,Word8)],
    properties :: Properties
} deriving (Show,Eq)

-- SUBACK
data SubackMessage = SubackMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    returnCodes :: [Word8],
    properties :: Properties
} deriving (Show,Eq)

-- UNSUBSCRIBE
data UnsubscribeMessage = UnsubscribeMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    topics :: [Text],
    properties :: Properties
} deriving (Show,Eq)

-- UNSUBACK
data UnsubackMessage = UnsubackMessage {
    fixedHeader :: FixedHeader,
    packetId :: Word16,
    properties :: Properties
} deriving (Show,Eq)

-- PINGREQ
data PingreqMessage = PingreqMessage {
    fixedHeader :: FixedHeader
} deriving (Show,Eq)

-- PINGRESP
data PingrespMessage = PingrespMessage {
    fixedHeader :: FixedHeader
} deriving (Show,Eq)

-- DISCONNECT
data DisconnectMessage = DisconnectMessage {
    fixedHeader :: FixedHeader,
    properties :: Properties
} deriving (Show,Eq)


-- Helper function for encoding remaining length (crucial for MQTT)
encodeRemainingLength :: Word32 -> ByteString
encodeRemainingLength len = 
  let rec acc n = 
        if n < 128 
          then pack $ (fromIntegral n):acc
          else rec ((fromIntegral n .&. 0x7f) : acc) (n `shiftR` 7)
  in rec [] len

