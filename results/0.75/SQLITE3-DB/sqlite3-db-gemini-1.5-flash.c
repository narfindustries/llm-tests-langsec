#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

// Structure to represent a Gemini capsule
typedef struct {
    int id;
    char name[255];
    char status[50];
} GeminiCapsule;


// Function to create the SQLite database and table
int createDatabase(const char *dbName) {
    sqlite3 *db;
    char *zErrMsg = 0;
    int rc;

    rc = sqlite3_open(dbName, &db);

    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        return 1;
    } else {
        char *sql = "CREATE TABLE IF NOT EXISTS gemini_capsules ("
                    "id INTEGER PRIMARY KEY AUTOINCREMENT,"
                    "name TEXT NOT NULL,"
                    "status TEXT);";

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
}


// Function to insert a Gemini capsule into the database
int insertCapsule(const char *dbName, GeminiCapsule capsule) {
    sqlite3 *db;
    char *zErrMsg = 0;
    int rc;
    char sql[512];

    rc = sqlite3_open(dbName, &db);
    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }

    snprintf(sql, sizeof(sql), "INSERT INTO gemini_capsules (name, status) VALUES ('%s', '%s');", capsule.name, capsule.status);

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
    // Create the database
    if (createDatabase("gemini_missions.db") != 0) {
        return 1;
    }

    // Insert some Gemini capsules
    GeminiCapsule capsule1 = {1, "Gemini 1", "Successful"};
    GeminiCapsule capsule2 = {2, "Gemini 2", "Successful"};
    GeminiCapsule capsule3 = {3, "Gemini 3", "Successful"};


    if (insertCapsule("gemini_missions.db", capsule1) != 0) return 1;
    if (insertCapsule("gemini_missions.db", capsule2) != 0) return 1;
    if (insertCapsule("gemini_missions.db", capsule3) != 0) return 1;


    printf("Data inserted successfully!\n");
    return 0;
}
