#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
int main() {

  char str[100];
  int listen_fd, comm_fd;

  struct sockaddr_in server_addr;

  listen_fd = socket(AF_INET, SOCK_STREAM, 0);

  bzero(&server_addr, sizeof(server_addr));

  server_addr.sin_family = AF_INET;

  // When INADDR_ANY is specified in the bind call,
  // the socket will be bound to all local interfaces
  server_addr.sin_addr.s_addr = htons(INADDR_ANY);

  server_addr.sin_port = htons(22000);

  int e = bind(listen_fd, (struct sockaddr *)&server_addr, sizeof(server_addr));

  if (e == -1) {
    fprintf(stderr, "Value of errno: %d\n", errno);
  }

  // sockfd-> socket file descriptor
  // 10 -> The backlog argument defines the maximum length to which the queue of pending connections for sockfd may grow.
  listen(listen_fd, 10);

  comm_fd = accept(listen_fd, (struct sockaddr *)NULL, NULL);

  while (1) {

    bzero(str, 100);

    read(comm_fd, str, 100);

    if (strcmp(str, "server_quit") == 0) {
      break;
    }

    printf("Echoing back - %s", str);

    write(comm_fd, str, strlen(str) + 1);
  }

  return 0;
}
