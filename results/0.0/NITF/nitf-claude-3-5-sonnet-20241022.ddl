def Main = {
  Sonnet;
}

def Sonnet = {
  Title;
  Author?;
  Lines;
}

def Title = {
  $line until $"\n";
}

def Author = {
  "By " $line until $"\n";
}

def Lines = {
  Line{14};
}

def Line = {
  $line until $"\n";
}