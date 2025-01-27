module MQTT {
  import DAEDALUS::Core;

  type Byte = UInt8;

  type FixedHeader = struct {
    messageType : UInt4;
    dupFlag     : UInt1;
    qosLevel    : UInt2;
    retain      : UInt1;
    remainingLength : VarInt;
  }

  type VarInt = struct {
    bytes : [Byte] &until(b -> (b & 0x80) == 0) &then(_ -> _.map(b -> b & 0x7F));
    value : UInt32 = this.bytes.foldl(0, (acc, b, i) -> acc | (b << (7 * i)));
  }

  type ConnectFlags = struct {
    reserved     : UInt1 = 0;
    cleanSession : UInt1;
    willFlag     : UInt1;
    willQoS      : UInt2;
    willRetain   : UInt1;
    passwordFlag : UInt1;
    userNameFlag : UInt1;
  }

  type ConnectPayload = struct {
    clientIdentifier : String;
    willTopic        : String &if(this._parent.flags.willFlag == 1);
    willMessage      : String &if(this._parent.flags.willFlag == 1);
    userName         : String &if(this._parent.flags.userNameFlag == 1);
    password         : String &if(this._parent.flags.passwordFlag == 1);
  }

  type Connect = struct {
    header  : FixedHeader;
    flags   : ConnectFlags;
    keepAlive : UInt16;
    payload : ConnectPayload;
  }

  type Publish = struct {
    header  : FixedHeader;
    topicName : String;
    packetIdentifier : UInt16 &if(this.header.qosLevel > 0);
    payload : [Byte];
  }

  type Subscribe = struct {
    header  : FixedHeader;
    packetIdentifier : UInt16;
    topicFilters : [Subscription] &until(_ -> _.isEmpty());
  }

  type Subscription = struct {
    topicFilter : String;
    requestedQoS : UInt8;
  }

  type Unsubscribe = struct {
    header  : FixedHeader;
    packetIdentifier : UInt16;
    topicFilters : [String] &until(_ -> _.isEmpty());
  }

  type MQTTMessage = union {
    connect    : Connect &when(this.header.messageType == 1);
    publish    : Publish &when(this.header.messageType == 3);
    subscribe  : Subscribe &when(this.header.messageType == 8);
    unsubscribe: Unsubscribe &when(this.header.messageType == 10);
  }

  type Packet = struct {
    header : FixedHeader;
    message: MQTTMessage;
  }

  type String = struct {
    length : UInt16;
    value  : [Byte] &length(this.length);
  }
}