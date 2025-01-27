module HL7v2;

import Daedalus.Core;
import Daedalus.Types;

type Message = {
  header: MessageHeader,
  segments: List<Segment>
};

type MessageHeader = {
  fieldSeparator: Text,
  encodingCharacters: Text,
  sendingApplication: Text,
  sendingFacility: Text,
  receivingApplication: Text,
  receivingFacility: Text,
  dateTimeOfMessage: DateTime,
  security: optional Text
};

type Segment = {
  name: Text,
  fields: List<Field>
};

type Field = {
  value: Text,
  repetitions: optional List<Text>,
  components: optional List<Text>
};

rule validateMessage(msg: Message) = 
  msg.header.fieldSeparator == "|" &&
  msg.header.encodingCharacters.length == 4;

parse Message from Text = 
  match split(input, "\r") {
    | (headerLine :: segmentLines) => 
      Message {
        header: parseHeader(headerLine),
        segments: map(parseSegment, segmentLines)
      }
    | _ => fail("Invalid message format")
  };

func parseHeader(line: Text): MessageHeader = 
  match split(line, "|") {
    | [_, enc, sender, sendFac, recv, recvFac, dateTime, security] =>
      MessageHeader {
        fieldSeparator: "|",
        encodingCharacters: enc,
        sendingApplication: sender,
        sendingFacility: sendFac,
        receivingApplication: recv,
        receivingFacility: recvFac,
        dateTimeOfMessage: parseDateTime(dateTime),
        security: optional(security)
      }
    | _ => fail("Invalid header format")
  };

func parseSegment(line: Text): Segment = 
  match split(line, "|") {
    | (segName :: fieldValues) =>
      Segment {
        name: segName,
        fields: map(parseField, fieldValues)
      }
    | _ => fail("Invalid segment format")
  };

func parseField(value: Text): Field = 
  Field {
    value: value,
    repetitions: optional(splitRepeats(value)),
    components: optional(splitComponents(value))
  };

func splitRepeats(value: Text): List<Text> = 
  split(value, "~");

func splitComponents(value: Text): List<Text> = 
  split(value, "^");

func parseDateTime(dateTime: Text): DateTime = 
  parseFormat(dateTime, "YYYYMMDDHHmmss");