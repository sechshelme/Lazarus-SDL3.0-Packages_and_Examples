#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#if defined(_WIN64)
#include <windows.h>
#endif

void WideChar_Test(const char *  str){
    printf("wchar_t size: %li\n",sizeof(wchar_t));
    printf("UTF8:    (%li)  ", strlen(str));
    for (int i=0; i<strlen(str); i++) {
      printf("%02X - ", str[i]);
    }
    printf("\n");

    size_t len = mbstowcs(nullptr, str, strlen(str));
    wchar_t * wc = (wchar_t *) malloc(len * sizeof(wchar_t));
    mbstowcs(wc, str, len);

    printf("WideStr: (%li)  ", len);
    for (int i=0; i<len; i++) {
      printf("%08X - ", wc[i]);
    }
    printf("\n\n");

    free(wc);
}

const char * TestString1 =  "12345678";
const char * TestString2 =  "ABCÄÖÜŸ";

#if defined(_WIN64)
void WideChar_Windows(const char *  str){
    printf("wchar_t size: %li\n",sizeof(wchar_t));
    printf("UTF8:    (%li)  ", strlen(str));
    for (int i=0; i<strlen(str); i++) {
      printf("%02X - ", str[i]);
    }
    printf("\n");

    size_t len = MultiByteToWideChar(CP_UTF8, 0, str, -1, nullptr, 0);
    wchar_t * wc = (wchar_t *) malloc(len * sizeof(wchar_t));
    MultiByteToWideChar( CP_UTF8, 0, str, -1, wc, len);

    printf("WideStr: (%li)  ", len);
    for (int i=0; i<len; i++) {
      printf("%04X - ", wc[i]);
    }
    printf("\n\n");

    free(wc);
}
#endif

int main(int argc, char **argv)
{
  printf("Testring 1: %s\n", TestString1);
  printf("Testring 2: %s\n", TestString2);
#if defined(_WIN64)
  printf("setlocale: %s\n", setlocale(LC_ALL, "German_Switzerland.utf8"));
#else
  printf("setlocale: %s\n", setlocale(LC_ALL, ""));
#endif

  printf("--- Testring 1 ---\n");
  WideChar_Test(TestString1);
  printf("--- Testring 2 ---\n");
  WideChar_Test(TestString2);

#if defined(_WIN64)
  printf("--- MultiByteToWideChar Testring 1 ---\n");
  WideChar_Windows(TestString1);
  printf("--- MultiByteToWideChar Testring 2 ---\n");
  WideChar_Windows(TestString2);
#endif

  return 0;
}
