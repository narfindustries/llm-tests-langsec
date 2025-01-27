#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

// Structure to represent a row in the database
typedef struct {
    int id;
    char name[100];
    char data[1000];
} Row;


// Function to create the database and table
int createDatabase(const char *dbName) {
    sqlite3 *db;
    char *zErrMsg = 0;
    int rc;

    rc = sqlite3_open(dbName, &db);

    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    char *sql = "CREATE TABLE IF NOT EXISTS data_table (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, data TEXT);";
    rc = sqlite3_exec(db, sql, 0, 0, &zErrMsg);

    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", zErrMsg);
        sqlite3_free(zErrMsg);
        sqlite3_close(db);
        return 1;
    }

    sqlite3_close(db);
    return 0;
}


// Function to insert a row into the database
int insertRow(const char *dbName, const Row *row) {
    sqlite3 *db;
    char *zErrMsg = 0;
    int rc;
    char sql[2048]; // Increased buffer size

    rc = sqlite3_open(dbName, &db);
    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    snprintf(sql, sizeof(sql), "INSERT INTO data_table (name, data) VALUES ('%s', '%s');", row->name, row->data);

    rc = sqlite3_exec(db, sql, 0, 0, &zErrMsg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", zErrMsg);
        sqlite3_free(zErrMsg);
        sqlite3_close(db);
        return 1;
    }

    sqlite3_close(db);
    return 0;
}


int main() {
    const char *dbName = "mydatabase.db";
    createDatabase(dbName);

    Row row1 = {0, "Row 1", "This is the data for row 1."};
    Row row2 = {0, "Row 2", "This is the data for row 2."};

    insertRow(dbName, &row1);
    insertRow(dbName, &row2);

    return 0;
}
