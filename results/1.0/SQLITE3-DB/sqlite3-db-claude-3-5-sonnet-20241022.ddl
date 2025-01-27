module sqlite3-db-claude-3-5-sonnet-20241022 (
    SQLite3DB
)

@encoding("aligned")
def SQLite3DB =
    Header
    TableDefinitions[]

def Header =
    Magic
    uint16BE(1)
    uint16BE(1)
    uint8(64)
    uint8(32)
    uint32BE(0)

def Magic =
    "SQLite format 3\x00"

def TableDefinitions[] =
    List { TableDefinition }*

def TableDefinition =
    uint32BE(1)  -- Page Type
    uint16BE(0)  -- Free block offset
    uint16BE     -- Number of cells
    uint16BE     -- Cell content offset
    uint8        -- Number of free bytes
    SchemaData

def SchemaData =
    "CREATE TABLE " Name
    "(" ColumnDefinitions ")"
    ";"

def Name =
    [A-Za-z] [A-Za-z0-9_]*

def ColumnDefinitions =
    List { ColumnDefinition /","/ }+

def ColumnDefinition =
    Name " " DataType Constraints*

def DataType =
    "INTEGER" | "TEXT" | "BLOB" | "REAL" | "NUMERIC"

def Constraints =
    " " ("PRIMARY KEY" |
         "NOT NULL" |
         "UNIQUE" |
         "DEFAULT" Value |
         "CHECK" "(" Expression ")")

def Value =
    Number | String | "NULL"

def Number =
    [0-9]+ ("." [0-9]+)?

def String =
    "'" ([^'] | "''")* "'"

def Expression =
    Name " " Operator " " Value

def Operator =
    "=" | ">" | "<" | ">=" | "<=" | "!=" | "LIKE"