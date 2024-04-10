  $ ./demoInterpret.exe << EOF
  > int factorial(int n) {
  >  if (n >= 1) {
  >   return n * factorial(n - 1);
  >  }
  >  else {
  >   return 1;
  >  }
  > }
  > int main() { 
  >  int n = 5;
  >  return factorial(n);
  > }
  > EOF
  120

  $ ./demoInterpret.exe << EOF
  >  int binarySearch(int a, int *array, int n) {
  >   int low = 0;
  >   int high = n - 1;
  >   int middle;
  >   while (low <= high) {
  >     middle = (low + high) / 2;
  >     if (a < array[middle] || a > array[middle]) {
  >       if (a < array[middle]) {
  >         high = middle - 1;
  >       } 
  >       else {
  >         low = middle + 1;
  >       }
  >     } 
  >     else {
  >       return middle;
  >     } 
  >   }
  >   return -1;
  > }
  > 
  > int main() {
  >   int array[5] = {3, 7, 10, 23, 100};
  >   return binarySearch(10, array, 5);
  > }
  > EOF
  2

  $ ./demoInterpret.exe << EOF
  > int main() {
  > int32_t i = 0;
  > int32_t j;
  > int32_t sum = 0;
  > while (i < 10) {
  >     j = 0;
  >     while (j < 10) {
  >         if (j == 5) {
  >             break;
  >         }
  >         sum = sum + 1;
  >         j = j + 1;
  >     }
  >     if (i == 5) {
  >         break;
  >     }
  >     i = i + 1;
  > }
  > return sum;
  > }
  > EOF
  30

  $ ./demoInterpret.exe << EOF
  > int memcpy(int8_t* source, int8_t* dst, int n) {
  >   int8_t i = 0;
  >   while (i < n) {
  >       dst[i] = source[i];
  >       i = i + 1;
  >   }
  >   return 0;
  > }
  > int main() {
  >   int8_t dst[4] = {0, 0, 0, 0};
  >   int32_t source[1] = {197121};
  >   memcpy(source, dst, 4);
  >   return dst[2];
  > }
  > EOF
  3

  $ ./demoInterpret.exe << EOF
  > int change(int32_t *pointer_a) {
  >   int32_t* pointer_a_2 = pointer_a;
  >   *pointer_a_2 = 100;
  >   return 0;
  > }
  > 
  > int main() {
  >   int32_t a = 20;
  >   change(&a);
  >   return a;
  > }
  > EOF
  100

  $ ./demoInterpret.exe << EOF
  > int main() {
  >   int32_t number;
  >   int8_t* a = malloc(10 * sizeof(int8_t));
  >   a[5] = 10;
  >   number = a[5];
  >   free(a);
  >   return a[5];
  > }
  > EOF
  Unknown variable with name - a

  $ ./demoInterpret.exe << EOF
  > int main() {
  >   int32_t i;
  >   int16_t a[5] = {0, 0, 0, 0, 0};
  >   for (i = 0; i < 5;) {
  >       a[i] = 5 - i;
  >       i = i + 1;
  >   }
  >   return a[0];
  > }
  > EOF
  5

  $ ./demoInterpret.exe << EOF
  > char main() {
  >   int n = 1;
  >   n = (n << 7) >> 1;
  >   char A = n + 1;
  >   if (!(A % 2 != 0) && A > 65) {
  >     return 97;
  >   } 
  >   return A + 25;
  > }
  > EOF
  Z

  $ ./demoInterpret.exe << EOF
  > int16_t main() {
  >   int count = 0;
  >   count = --(-(++(count + 2)));
  >   return count;
  > }
  > EOF
  -4

  $ ./demoInterpret.exe << EOF
  > int main() {
  >   int16_t number = 333;
  >   int16_t* pointer = &number;
  >   int16_t* pointer_2 = pointer;
  >   *pointer_2 = 666;
  >   return number;
  > }
  > EOF
  666

  $ ./demoInterpret.exe << EOF
  > int overflowMe(int n) {
  >   return overflowMe(n + 1);
  > }
  > 
  > int main() {
  >   return overflowMe(0);
  > }
  Stack overflow!


