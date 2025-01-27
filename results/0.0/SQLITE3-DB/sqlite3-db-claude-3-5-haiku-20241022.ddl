module SQLITE3-DB

import Prelude
import Data.List
import Data.Maybe
import Data.Text

type DatabaseConnection = {
    host: Text,
    port: Int,
    username: Text,
    password: Text
}

type SQLiteTable = {
    name: Text,
    columns: List Column
}

type Column = {
    columnName: Text,
    dataType: ColumnType,
    isPrimaryKey: Bool,
    isNullable: Bool
}

enum ColumnType {
    INTEGER,
    TEXT,
    REAL,
    BLOB,
    DATETIME
}

type QueryResult = {
    rows: List (List Text),
    columnNames: List Text
}

interface SQLiteDatabase {
    connect(config: DatabaseConnection) -> Maybe DatabaseConnection
    disconnect(connection: DatabaseConnection) -> Bool
    createTable(connection: DatabaseConnection, table: SQLiteTable) -> Bool
    insertRecord(connection: DatabaseConnection, tableName: Text, record: List Text) -> Bool
    executeQuery(connection: DatabaseConnection, query: Text) -> Maybe QueryResult
    updateRecord(connection: DatabaseConnection, tableName: Text, 
                 updateValues: List Text, 
                 whereCondition: Text) -> Bool
    deleteRecord(connection: DatabaseConnection, tableName: Text, whereCondition: Text) -> Bool
}

implementation SQLiteDatabase {
    connect(config) = 
        if validateConnection(config) 
        then Just config 
        else Nothing

    disconnect(connection) = 
        validateConnection(connection)

    createTable(connection, table) = 
        validateConnection(connection) && 
        validateTableDefinition(table)

    insertRecord(connection, tableName, record) = 
        validateConnection(connection) && 
        not (null record)

    executeQuery(connection, query) = 
        if validateConnection(connection) && not (null query)
        then Just {
            rows = [["sample", "data"]],
            columnNames = ["column1", "column2"]
        }
        else Nothing

    updateRecord(connection, tableName, updateValues, whereCondition) = 
        validateConnection(connection) && 
        not (null updateValues)

    deleteRecord(connection, tableName, whereCondition) = 
        validateConnection(connection)
}

def validateConnection(connection: DatabaseConnection) -> Bool = 
    not (null connection.host) && 
    connection.port > 0 && 
    not (null connection.username)

def validateTableDefinition(table: SQLiteTable) -> Bool = 
    not (null table.name) && 
    not (null table.columns)