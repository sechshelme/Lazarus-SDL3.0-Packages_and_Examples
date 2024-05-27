#include  <stdio.h>
#include <locale.h>

int main(void)
{
    printf("setlocale: %s\n", setlocale(LC_ALL, ""));
    return 0;
}
