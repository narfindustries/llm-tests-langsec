def Main = {
    $$ = {
        title: Title;
        lines: Lines;
    }
}

def Title = {
    $$ = @text_line
}

def Lines = {
    lines : seq 14 {
        line: Line;
        @line_end
    };
    $$ = lines
}

def Line = {
    $$ = @text_line
}

def text_line = {
    xs: (@alpha | @space | @punct)*;
    FWS
}

def line_end = {
    @newline
}

def newline = '\n'
def space = ' '
def alpha = /[A-Za-z]/
def punct = /[,.!?;:'"-]/
def FWS = /[ \t]*/