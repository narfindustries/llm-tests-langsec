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
        //Simple parsing, assuming comma separated values.  Error handling omitted for brevity.
        char* txid = strtok(line, ",");
        char* amountStr = strtok(NULL, ",");
        char* address = strtok(NULL, ",");

        if(txid && amountStr && address){
            (*numTransactions)++;
            transactions = realloc(transactions, (*numTransactions) * sizeof(Transaction));
            if(transactions == NULL){
                perror("Memory allocation failed");
                fclose(file);
                return NULL;
            }
            strncpy(transactions[*numTransactions -1].txid, txid, sizeof(transactions[*numTransactions -1].txid) -1);
            transactions[*numTransactions -1].txid[sizeof(transactions[*numTransactions -1].txid) -1] = '\0';
            transactions[*numTransactions -1].amount = atof(amountStr);
            strncpy(transactions[*numTransactions -1].address, address, sizeof(transactions[*numTransactions -1].address) -1);
            transactions[*numTransactions -1].address[sizeof(transactions[*numTransactions -1].address) -1] = '\0';

        }
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
        printf("Transaction ID: %s, Amount: %.2f, Address: %s\n",
               transactions[i].txid, transactions[i].amount, transactions[i].address);
    }

    free(transactions);
    return 0;
}

//transactions.csv example content:
//e2e7f9137b78966178020c661566510226724c25266222466646666666666666,1.2345,bc1q00000000000000000000000000000000000000
//a1b2c3d4e5f678901234567890abcdef0123456789abcdef,0.5,bc1q11111111111111111111111111111111111111
