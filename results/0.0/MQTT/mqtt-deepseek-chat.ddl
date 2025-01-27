module MQTT where

import Daedalus

-- MQTT Packet Types
data PacketType = CONNECT | CONNACK | PUBLISH | PUBACK | PUBREC | PUBREL | PUBCOMP | SUBSCRIBE | SUBACK | UNSUBSCRIBE | UNSUBACK | PINGREQ | PINGRESP | DISCONNECT
  deriving (Show, Eq)

-- MQTT Fixed Header
data FixedHeader = FixedHeader
  { packetType :: PacketType
  , flags      :: Word8
  , remainingLength :: Word32
  } deriving (Show)

-- MQTT Connect Flags
data ConnectFlags = ConnectFlags
  { usernameFlag :: Bool
  , passwordFlag :: Bool
  , willRetain   :: Bool
  , willQoS      :: Word8
  , willFlag     :: Bool
  , cleanSession :: Bool
  , reserved     :: Bool
  } deriving (Show)

-- MQTT Connect Packet
data ConnectPacket = ConnectPacket
  { protocolName  :: String
  , protocolLevel :: Word8
  , connectFlags  :: ConnectFlags
  , keepAlive     :: Word16
  , clientId      :: String
  , willTopic     :: Maybe String
  , willMessage   :: Maybe String
  , username      :: Maybe String
  , password      :: Maybe String
  } deriving (Show)

-- MQTT ConnAck Packet
data ConnAckPacket = ConnAckPacket
  { sessionPresent :: Bool
  , returnCode     :: Word8
  } deriving (Show)

-- MQTT Publish Packet
data PublishPacket = PublishPacket
  { topicName :: String
  , packetId  :: Maybe Word16
  , payload   :: String
  } deriving (Show)

-- MQTT Subscribe Packet
data SubscribePacket = SubscribePacket
  { packetId      :: Word16
  , topicFilters  :: [(String, Word8)]
  } deriving (Show)

-- MQTT SubAck Packet
data SubAckPacket = SubAckPacket
  { packetId     :: Word16
  , returnCodes  :: [Word8]
  } deriving (Show)

-- MQTT Unsubscribe Packet
data UnsubscribePacket = UnsubscribePacket
  { packetId     :: Word16
  , topicFilters :: [String]
  } deriving (Show)

-- MQTT UnsubAck Packet
data UnsubAckPacket = UnsubAckPacket
  { packetId :: Word16
  } deriving (Show)

-- MQTT PingReq Packet
data PingReqPacket = PingReqPacket
  deriving (Show)

-- MQTT PingResp Packet
data PingRespPacket = PingRespPacket
  deriving (Show)

-- MQTT Disconnect Packet
data DisconnectPacket = DisconnectPacket
  deriving (Show)

-- MQTT Packet
data MQTTPacket
  = MQTTConnect ConnectPacket
  | MQTTConnAck ConnAckPacket
  | MQTTPublish PublishPacket
  | MQTTPubAck Word16
  | MQTTPubRec Word16
  | MQTTPubRel Word16
  | MQTTPubComp Word16
  | MQTTSubscribe SubscribePacket
  | MQTTSubAck SubAckPacket
  | MQTTUnsubscribe UnsubscribePacket
  | MQTTUnsubAck UnsubAckPacket
  | MQTTPingReq PingReqPacket
  | MQTTPingResp PingRespPacket
  | MQTTDisconnect DisconnectPacket
  deriving (Show)

-- Parser for Fixed Header
fixedHeader :: Parser FixedHeader
fixedHeader = do
  pt <- packetTypeParser
  fl <- byte
  rl <- remainingLengthParser
  return $ FixedHeader pt fl rl

-- Parser for Packet Type
packetTypeParser :: Parser PacketType
packetTypeParser = do
  b <- byte
  case b `shiftR` 4 of
    1  -> return CONNECT
    2  -> return CONNACK
    3  -> return PUBLISH
    4  -> return PUBACK
    5  -> return PUBREC
    6  -> return PUBREL
    7  -> return PUBCOMP
    8  -> return SUBSCRIBE
    9  -> return SUBACK
    10 -> return UNSUBSCRIBE
    11 -> return UNSUBACK
    12 -> return PINGREQ
    13 -> return PINGRESP
    14 -> return DISCONNECT
    _  -> fail "Invalid packet type"

-- Parser for Remaining Length
remainingLengthParser :: Parser Word32
remainingLengthParser = do
  len <- varInt
  return $ fromIntegral len

-- Parser for Variable Length Integer
varInt :: Parser Word32
varInt = do
  b <- byte
  if b < 128
    then return $ fromIntegral b
    else do
      rest <- varInt
      return $ (fromIntegral b .&. 0x7F) .|. (rest `shiftL` 7)

-- Parser for Connect Packet
connectPacket :: Parser ConnectPacket
connectPacket = do
  protocolName <- utf8String
  protocolLevel <- byte
  connectFlags <- connectFlagsParser
  keepAlive <- word16be
  clientId <- utf8String
  willTopic <- optional utf8String
  willMessage <- optional utf8String
  username <- optional utf8String
  password <- optional utf8String
  return $ ConnectPacket protocolName protocolLevel connectFlags keepAlive clientId willTopic willMessage username password

-- Parser for Connect Flags
connectFlagsParser :: Parser ConnectFlags
connectFlagsParser = do
  flags <- byte
  return $ ConnectFlags
    { usernameFlag = testBit flags 7
    , passwordFlag = testBit flags 6
    , willRetain   = testBit flags 5
    , willQoS      = (flags `shiftR` 3) .&. 0x03
    , willFlag     = testBit flags 2
    , cleanSession = testBit flags 1
    , reserved     = testBit flags 0
    }

-- Parser for UTF-8 String
utf8String :: Parser String
utf8String = do
  len <- word16be
  bytes <- bytes len
  return $ decodeUtf8 bytes

-- Parser for MQTT Packet
mqttPacket :: Parser MQTTPacket
mqttPacket = do
  header <- fixedHeader
  case packetType header of
    CONNECT     -> MQTTConnect <$> connectPacket
    CONNACK     -> MQTTConnAck <$> connAckPacket
    PUBLISH     -> MQTTPublish <$> publishPacket
    PUBACK      -> MQTTPubAck <$> word16be
    PUBREC      -> MQTTPubRec <$> word16be
    PUBREL      -> MQTTPubRel <$> word16be
    PUBCOMP     -> MQTTPubComp <$> word16be
    SUBSCRIBE   -> MQTTSubscribe <$> subscribePacket
    SUBACK      -> MQTTSubAck <$> subAckPacket
    UNSUBSCRIBE -> MQTTUnsubscribe <$> unsubscribePacket
    UNSUBACK    -> MQTTUnsubAck <$> word16be
    PINGREQ     -> return MQTTPingReq
    PINGRESP    -> return MQTTPingResp
    DISCONNECT  -> return MQTTDisconnect
    _           -> fail "Unsupported packet type"

-- Parser for ConnAck Packet
connAckPacket :: Parser ConnAckPacket
connAckPacket = do
  sessionPresent <- byte
  returnCode <- byte
  return $ ConnAckPacket (testBit sessionPresent 0) returnCode

-- Parser for Publish Packet
publishPacket :: Parser PublishPacket
publishPacket = do
  topicName <- utf8String
  packetId <- optional word16be
  payload <- bytes $ remainingLength - fromIntegral (length topicName) - maybe 0 (const 2) packetId
  return $ PublishPacket topicName packetId (decodeUtf8 payload)

-- Parser for Subscribe Packet
subscribePacket :: Parser SubscribePacket
subscribePacket = do
  packetId <- word16be
  topicFilters <- many $ do
    topic <- utf8String
    qos <- byte
    return (topic, qos)
  return $ SubscribePacket packetId topicFilters

-- Parser for SubAck Packet
subAckPacket :: Parser SubAckPacket
subAckPacket = do
  packetId <- word16be
  returnCodes <- many byte
  return $ SubAckPacket packetId returnCodes

-- Parser for Unsubscribe Packet
unsubscribePacket :: Parser UnsubscribePacket
unsubscribePacket = do
  packetId <- word16be
  topicFilters <- many utf8String
  return $ UnsubscribePacket packetId topicFilters

-- Parser for UnsubAck Packet
unsubAckPacket :: Parser UnsubAckPacket
unsubAckPacket = do
  packetId <- word16be
  return $ UnsubAckPacket packetId

-- Parser for PingReq Packet
pingReqPacket :: Parser PingReqPacket
pingReqPacket = return PingReqPacket

-- Parser for PingResp Packet
pingRespPacket :: Parser PingRespPacket
pingRespPacket = return PingRespPacket

-- Parser for Disconnect Packet
disconnectPacket :: Parser DisconnectPacket
disconnectPacket = return DisconnectPacket

-- Main Parser
parseMQTT :: Parser MQTTPacket
parseMQTT = mqttPacket