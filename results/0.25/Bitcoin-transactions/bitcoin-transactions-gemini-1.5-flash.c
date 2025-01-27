#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a Bitcoin transaction
typedef struct {
    char txid[65]; // Transaction ID (64 hex characters + null terminator)
    double amount;
    char address[35]; // Bitcoin address (34 characters + null terminator)
} Transaction;


// Function to read transactions from a file
Transaction* readTransactions(const char* filename, int* numTransactions) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        return NULL;
    }

    char line[1024];
    *numTransactions = 0;
    Transaction* transactions = NULL;

    while (fgets(line, sizeof(line), file) != NULL) {
        // Assuming CSV format: txid,amount,address
        char* txid = strtok(line, ",");
        char* amountStr = strtok(NULL, ",");
        char* address = strtok(NULL, ",");

        if (txid == NULL || amountStr == NULL || address == NULL) {
            fprintf(stderr, "Invalid transaction format in line: %s", line);
            continue; // Skip invalid lines
        }

        //Reallocate memory for transactions array
        (*numTransactions)++;
        transactions = (Transaction*)realloc(transactions, (*numTransactions) * sizeof(Transaction));
        if(transactions == NULL){
          perror("Memory allocation failed");
          fclose(file);
          return NULL;
        }

        strncpy(transactions[*numTransactions - 1].txid, txid, sizeof(transactions[*numTransactions - 1].txid) -1);
        transactions[*numTransactions - 1].txid[sizeof(transactions[*numTransactions - 1].txid) -1] = '\0'; //Ensure null termination

        transactions[*numTransactions - 1].amount = atof(amountStr);
        strncpy(transactions[*numTransactions - 1].address, address, sizeof(transactions[*numTransactions - 1].address) -1);
        transactions[*numTransactions - 1].address[sizeof(transactions[*numTransactions - 1].address) -1] = '\0'; //Ensure null termination

    }

    fclose(file);
    return transactions;
}


int main() {
    int numTransactions;
    Transaction* transactions = readTransactions("transactions.csv", &numTransactions);

    if (transactions == NULL) {
        return 1; // Indicate an error
    }

    // Process transactions (example: print them)
    for (int i = 0; i < numTransactions; i++) {
        printf("Transaction ID: %s\n", transactions[i].txid);
        printf("Amount: %.8f\n", transactions[i].amount);
        printf("Address: %s\n", transactions[i].address);
        printf("--------------------\n");
    }

    free(transactions); // Free dynamically allocated memory
    return 0;
}
