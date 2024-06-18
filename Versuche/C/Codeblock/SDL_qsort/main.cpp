#include <SDL3/SDL.h>

#define array_len 50

static int num_compare(const void *para1, const void *para2)
{
    int a = *(int *) para1;
    int b = *(int *) para2;
    return (a > b);
}

int main(int argc, char *argv[])
{
    int nums[array_len];
    for (int i = 0; i < array_len; i++) {
      nums[i] = SDL_rand() % 50;
//      nums[i] = array_len -  i*2;
      SDL_Log("%i  %i",i, nums[i]);
    }

    SDL_qsort(nums, array_len, sizeof (int), num_compare);

    for (int i = 0; i < array_len; i++) {
      SDL_Log("%i", nums[i]);
    }
    return 0;
}
