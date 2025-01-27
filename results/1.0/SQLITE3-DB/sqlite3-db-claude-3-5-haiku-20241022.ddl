specification SQLITE3-DB {
    type Database {
        name: string,
        tables: list of Table
    }

    type Table {
        name: string,
        columns: list of Column,
        constraints: list of Constraint
    }

    type Column {
        name: string,
        dataType: ColumnType,
        nullable: boolean,
        primaryKey: boolean,
        unique: boolean,
        defaultValue: optional string
    }

    enum ColumnType {
        INTEGER,
        TEXT, 
        REAL,
        BLOB,
        NUMERIC
    }

    type Constraint {
        type: ConstraintType,
        details: string
    }

    enum ConstraintType {
        FOREIGN_KEY,
        CHECK,
        UNIQUE
    }

    function createDatabase(name: string) -> Database {
        return Database {
            name: name,
            tables: []
        }
    }

    function addTable(database: Database, tableName: string) -> Database {
        let newTable = Table {
            name: tableName,
            columns: [],
            constraints: []
        };
        database.tables.append(newTable);
        return database
    }

    function addColumn(table: Table, 
                       columnName: string, 
                       dataType: ColumnType, 
                       nullable: boolean = true,
                       primaryKey: boolean = false,
                       unique: boolean = false,
                       defaultValue: optional string = null) -> Table {
        let newColumn = Column {
            name: columnName,
            dataType: dataType,
            nullable: nullable,
            primaryKey: primaryKey,
            unique: unique,
            defaultValue: defaultValue
        };
        table.columns.append(newColumn);
        return table
    }

    function addForeignKeyConstraint(table: Table, 
                                     columnName: string, 
                                     referencedTable: string, 
                                     referencedColumn: string) -> Table {
        let constraint = Constraint {
            type: ConstraintType.FOREIGN_KEY,
            details: "FOREIGN KEY (" + columnName + ") REFERENCES " + referencedTable + "(" + referencedColumn + ")"
        };
        table.constraints.append(constraint);
        return table
    }
}