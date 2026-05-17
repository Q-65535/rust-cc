#include<stdio.h>

int a = 1;
int b =  sizeof(a);

int main(void) {
    {
        int a = 6;
        {
            int a = 3;
        }
    }
}