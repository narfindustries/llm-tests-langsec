def Main = {
  result: sonnet = {
    --Enforce start pattern
    $"Sonnet"
    $" "
    $"#"
    number: uint8
    --Extract sonnet number
    FWS
    lines: many1 {
      text: /[^\n\r]+/   --Capture line content
      EOL
    }
    --Validate line count and structure
    @assert |lines| == 14
    @assert |lines[0..4]|   == 4  --First quatrain
    @assert |lines[4..8]|   == 4  --Second quatrain
    @assert |lines[8..11]|  == 3  --First tercet
    @assert |lines[11..14]| == 3  --Second tercet
  }
  return result
}

def EOL = {| $"\n" | $"\r\n" | $"\r" |}
def FWS = many { $" " | $"\t" }

def sonnet = {
  number: uint8
  lines: lines[14]
}

def lines = [String]

def uint8 = /[0-9]{1,3}/