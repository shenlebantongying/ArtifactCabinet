#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

int main(int argc,char **argv)
{
  int slbSocket;
  char send_buffer[100];
  char recv_buffer[100];
  struct sockaddr_in server_addr;

  // AF_INET =>IPv4 Internet protocols

  slbSocket = socket(AF_INET,SOCK_STREAM,0);
  bzero(&server_addr,sizeof server_addr);

  server_addr.sin_family=AF_INET;
  server_addr.sin_port=htons(22000);

  inet_pton(AF_INET,"127.0.0.1",&(server_addr.sin_addr));

  int e = connect(slbSocket,(struct sockaddr *)&server_addr,sizeof(server_addr));

  if (e==-1){
    fprintf(stderr, "Value of errno: %d\n", errno);
    return 1;

  }

  while(1)
  {
    bzero( send_buffer, 100);
    bzero( recv_buffer, 100);

    fscanf(stdin,"%s",send_buffer);

    write(slbSocket, send_buffer,strlen(send_buffer)+1);
    read(slbSocket, recv_buffer,100);

    if (strcmp(recv_buffer,"exit") ==0 || strcmp(recv_buffer,"quit") == 0){
      break;
    }

    printf("%s", recv_buffer);
  }

}
