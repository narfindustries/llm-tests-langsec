domain MQTT {
  types {
    uint8  : 1 byte;
    uint16 : 2 bytes;
    uint32 : 4 bytes;
    string : sequence of uint8;
  }

  grammar {
    MQTTMessage : 
      (FixedHeader, 
       (Publish | PubAck | PubRec | PubRel | PubComp | Subscribe | SubAck | Unsubscribe | UnsubAck | PingReq | PingResp | Disconnect)
      );

    FixedHeader : 
      (uint8:4, uint8, uint8:7, uint8);

    Publish : 
      (uint16, string, (string, string)?:optional);

    PubAck : 
      (uint16);

    PubRec : 
      (uint16);

    PubRel : 
      (uint16);

    PubComp : 
      (uint16);

    Subscribe : 
      (uint16, (string, uint8)?:optional);

    SubAck : 
      (uint16, (uint8)?:optional);

    Unsubscribe : 
      (uint16, (string)?:optional);

    UnsubAck : 
      (uint16);

    PingReq : 
      ();

    PingResp : 
      ();

    Disconnect : 
      ();
  }
}