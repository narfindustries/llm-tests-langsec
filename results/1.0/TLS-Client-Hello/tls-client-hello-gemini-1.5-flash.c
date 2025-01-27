#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#define SERVER_IP "127.0.0.1"
#define SERVER_PORT 8443

int main() {
    SSL_CTX *ctx;
    SSL *ssl;
    int sockfd;
    struct sockaddr_in serv_addr;

    // Initialize OpenSSL
    SSL_library_init();
    SSL_load_error_strings();
    OpenSSL_add_all_algorithms();

    // Create SSL context
    ctx = SSL_CTX_new(TLS_client_method());
    if (ctx == NULL) {
        ERR_print_errors_fp(stderr);
        return 1;
    }

    // Create socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("Error opening socket");
        return 1;
    }

    // Set up server address
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(SERVER_PORT);
    if (inet_pton(AF_INET, SERVER_IP, &serv_addr.sin_addr) <= 0) {
        perror("Invalid address/ Address not supported");
        return 1;
    }

    // Connect to server
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        perror("Error connecting");
        return 1;
    }

    // Create SSL object
    ssl = SSL_new(ctx);
    if (ssl == NULL) {
        ERR_print_errors_fp(stderr);
        return 1;
    }

    // Connect SSL to socket
    if (SSL_set_fd(ssl, sockfd) == 0) {
        ERR_print_errors_fp(stderr);
        return 1;
    }

    // Perform SSL handshake
    if (SSL_connect(ssl) <= 0) {
        ERR_print_errors_fp(stderr);
        return 1;
    }

    printf("Connected to server!\n");

    //Send and receive data (example)

    char buffer[1024] = "Hello from client!";
    int bytes_sent = SSL_write(ssl, buffer, strlen(buffer));
    if (bytes_sent <= 0){
        ERR_print_errors_fp(stderr);
        return 1;
    }
    char buffer_recv[1024];
    int bytes_recv = SSL_read(ssl, buffer_recv, sizeof(buffer_recv));
    if (bytes_recv <= 0){
        ERR_print_errors_fp(stderr);
        return 1;
    }
    buffer_recv[bytes_recv] = '\0';
    printf("Received from server: %s\n", buffer_recv);



    // Clean up
    SSL_free(ssl);
    SSL_CTX_free(ctx);
    close(sockfd);

    return 0;
}
