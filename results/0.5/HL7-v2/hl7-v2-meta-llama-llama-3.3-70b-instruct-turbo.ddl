domain HL7_v2 {
  import HL7_v2_Segment;

  grammar HL7_v2_Message {
    rule message = 
      (seg: Segment)*
    ;

    rule Segment = 
      (MSH | EVN | PID | PV1 | OBR)*
    ;

    rule MSH = 
      "MSH" ~ 
      (fields: Field[1-])?
    ;

    rule EVN = 
      "EVN" ~ 
      (fields: Field[1-])?
    ;

    rule PID = 
      "PID" ~ 
      (fields: Field[1-])?
    ;

    rule PV1 = 
      "PV1" ~ 
      (fields: Field[1-])?
    ;

    rule OBR = 
      "OBR" ~ 
      (fields: Field[1-])?
    ;

    rule Field = 
      (component: Component[1-])?
    ;

    rule Component = 
      (subcomponent: Subcomponent[1-])?
    ;

    rule Subcomponent = 
      ([^~\]+)
    ;
  }
}