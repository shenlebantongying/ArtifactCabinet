#include <stdio.h>

int main() {
    int n = 0;
    scanf("%d", &n);

    while (n>0){
        n = n-1;
        int i = 0;
        scanf("%d",&i);
        int x_min = 0;
        int x_max = 0;
        int y_min = 0;
        int y_max = 0;
        while (i > 0){
            i = i -1;
            int t_x,t_y = 0;
            scanf("%d %d",&t_x,&t_y);
            if (t_x>x_max){x_max = t_x;}
            if (t_x<x_min){x_min = t_x;}
            if (t_y>y_max){y_max = t_y;}
            if (t_y<y_min){y_min = t_y;}
        }
        printf("%d\n",2*(x_max-x_min+y_max-y_min));
    }

    return 0;
}
