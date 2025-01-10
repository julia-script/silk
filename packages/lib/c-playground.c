// C Program to illustrate the dereferencing of pointer
#include <stdio.h>

int main() {
  // Declare integer variable number
  int num = 10;
  int numb = num;
  printf("numb: %d\n", &numb);
  printf("num: %d\n", &num);

  // Declare pointer to store address of number
  // int* ptr = &num;

  // Print the value of number
  // printf("Value of num = %d \n", num);

  // // Print Address of the number using & operator
  // printf("Address of num = %d \n", &num);
  // printf("Address of num = %d \n", &num);

  // // Print Address stored in the pointer
  // printf("Address stored in the ptr = %p \n\n", ptr);

  // printf("Dereference content in ptr using *ptr\n\n");

  // // Access the content using * operator
  // printf("Value of *ptr = %d \n", *ptr);

  // printf("Now, *ptr is same as number\n\n");

  // printf("Modify the value using pointer to 6 \n\n");

  // // Modify the content in the address to 6 using pointer
  // *ptr = 6;

  // // Print the modified value using pointer
  // printf("Value of *ptr = %d \n", *ptr);

  // // Print the modified value using variable
  // printf("Value of number = %d \n", num);

  return 0;
}