domain HL7v2 {
  encoding = "ASCII";
  newline = "\r";

  type Message = Sequence [
    { name: "MSH", type: Segment_MSH },
    Choice [
      { name: "ERR", type: Segment_ERR },
      Repeat(0:, Segment),
    ],
  ];

  type Segment = Choice [
    { name: "MSH", type: Segment_MSH },
    { name: "ERR", type: Segment_ERR },
    { name: "PID", type: Segment_PID },
    { name: "PV1", type: Segment_PV1 },
  ];

  type Segment_MSH = Sequence [
    { name: "Field Separator", type: byte(124) },
    { name: "Sending Facility", type: string(3) },
    { name: "Sending Application", type: string(3) },
    { name: "Date", type: date },
    { name: "Time", type: time },
    { name: "Security", type: string },
    { name: "Message Type", type: string(3) },
    { name: "Trigger Event", type: string(3) },
    { name: "Message Control ID", type: string(20) },
  ];

  type Segment_ERR = Sequence [
    { name: "Segment ID", type: byte(69, 82, 82) },
    { name: "Error Code", type: byte(3) },
    { name: "Error Location", type: string },
  ];

  type Segment_PID = Sequence [
    { name: "Segment ID", type: byte(80, 73, 68) },
    { name: "Patient ID", type: string(20) },
    { name: "Patient Name", type: string(50) },
  ];

  type Segment_PV1 = Sequence [
    { name: "Segment ID", type: byte(80, 86, 49) },
    { name: "Patient Class", type: string(1) },
    { name: "Visit Number", type: string(20) },
  ];

  function date: integer {
    format: "YYYYMMDD"
  }

  function time: integer {
    format: "HHMMSS"
  }
}