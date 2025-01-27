module Main where

import Daedalus.Panic (panic)

-- Data types for MQTT packets

data MQTTPacket = ConnectPacket {
    clientID :: String,
    cleanSession :: Bool,
    keepAlive :: Word16
} | PublishPacket {
    topic :: String,
    qos :: Word8,
    retain :: Bool,
    payload :: ByteString
} | PubackPacket {
    packetID :: Word16
} | PubrecPacket {
    packetID :: Word16
} | PubrelPacket {
    packetID :: Word16
} | PubcompPacket {
    packetID :: Word16
} | SubscribePacket {
    topics :: [(String, Word8)],
    packetID :: Word16
} | SubackPacket {
    packetID :: Word16,
    grantedQoS :: [Word8]
} | UnsubscribePacket {
    topics :: [String],
    packetID :: Word16
} | UnsubackPacket {
    packetID :: Word16
} | PingreqPacket | PingrespPacket | DisconnectPacket

-- ByteString type for binary data

type ByteString = [Word8]

-- Helper functions for encoding and decoding

encodeWord8 :: Word8 -> ByteString
encodeWord8 x = [x]

encodeWord16 :: Word16 -> ByteString
encodeWord16 x = [fromIntegral (x `shiftR` 8), fromIntegral (x.&.0xFF)]

encodeString :: String -> ByteString
encodeString s = let len = length s in encodeWord16 (fromIntegral len) ++ map fromIntegral (s)

decodeWord8 :: ByteString -> Maybe (Word8, ByteString)
decodeWord8 (x:xs) = Just (x, xs)
decodeWord8 [] = Nothing

decodeWord16 :: ByteString -> Maybe (Word16, ByteString)
decodeWord16 (x:y:xs) = Just ((fromIntegral x `shiftL` 8) .|. fromIntegral y, xs)
decodeWord16 _ = Nothing

decodeString :: ByteString -> Maybe (String, ByteString)
decodeString bs = do
  (len, rest) <- decodeWord16 bs
  let len' = fromIntegral len
  if length rest >= len'
    then let (s, rest') = splitAt len' rest in Just (map (chr . fromIntegral) s, rest')
    else Nothing

-- Daedalus grammar for MQTT packets

mqttPacket :: Daedalus.Value MQTTPacket
mqttPacket = choice [
    ConnectPacket <$> (string "Connect" *> clientID <*> cleanSession <*> keepAlive),
    PublishPacket <$> (string "Publish" *> topic <*> qos <*> retain <*> payload),
    PubackPacket <$> (string "Puback" *> packetID),
    PubrecPacket <$> (string "Pubrec" *> packetID),
    PubrelPacket <$> (string "Pubrel" *> packetID),
    PubcompPacket <$> (string "Pubcomp" *> packetID),
    SubscribePacket <$> (string "Subscribe" *> topics <*> packetID),
    SubackPacket <$> (string "Suback" *> packetID <*> grantedQoS),
    UnsubscribePacket <$> (string "Unsubscribe" *> topics <*> packetID),
    UnsubackPacket <$> (string "Unsuback" *> packetID),
    PingreqPacket <$ string "Pingreq",
    PingrespPacket <$ string "Pingresp",
    DisconnectPacket <$ string "Disconnect"
    ]

clientID :: Daedalus.Value String
clientID = string "ClientID" *> some (byte)

cleanSession :: Daedalus.Value Bool
cleanSession = bool "CleanSession"

keepAlive :: Daedalus.Value Word16
keepAlive = word16 "KeepAlive"

topic :: Daedalus.Value String
topic = string "Topic" *> some (byte)

qos :: Daedalus.Value Word8
qos = word8 "QoS"

retain :: Daedalus.Value Bool
retain = bool "Retain"

payload :: Daedalus.Value ByteString
payload = bytes "Payload"

packetID :: Daedalus.Value Word16
packetID = word16 "PacketID"

topics :: Daedalus.Value [(String, Word8)]
topics = many (pair topic qos)

grantedQoS :: Daedalus.Value [Word8]
grantedQoS = many (word8 "GrantedQoS")


main :: Daedalus.Value ()
main = do
  p <- mqttPacket
  return ()

