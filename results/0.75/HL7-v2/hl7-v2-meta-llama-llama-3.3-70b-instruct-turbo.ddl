# HL7 v2 Meta Specification
definition hl7_v2_meta {
  # Import standard ISO dates
  import "iso_dates.ddl"

  # Define a HL7 v2 message structure
  structure hl7_message {
    # Message header
    field MSH: segment {
      # Field separator
      field.1: byte = '\x7c'
      # Encoding characters
      field.2: bytes = "^\~\\&"
      # Sending facility
      field.4: string
      # Sending application
      field.5: string
      # Receiving facility
      field.6: string
      # Receiving application
      field.7: string
      # Date and time
      field.7: date_time
    }

    # Patient information
    field PID: segment {
      # Patient ID
      field.3: string
      # Patient name
      field.5: string
      # Date of birth
      field.7: date
      # Sex
      field.8: string
    }

    # Visit information
    field PV1: segment {
      # Visit number
      field.1: string
      # Admission date and time
      field.4: date_time
      # Discharge date and time
      field.6: date_time
    }
  }

  # Define a grammar for HL7 v2 messages
  grammar hl7_v2_grammar {
    rule message: hl7_message
    rule segment: MSH | PID | PV1
  }
}

# Define the version of the HL7 v2 standard
const hl7_v2_version = "2.3"

# Define the character encoding
const character_encoding = "UTF-8"

# Define the structure for the output
structure output_structure {
  field message: hl7_message
}

# Define the grammar for the output
grammar output_grammar {
  rule message: output_structure
}

# Compile the specification
compile specification hl7_v2_meta for output hl7_v2_grammar version hl7_v2_version character_encoding character_encoding output_structure output_structure