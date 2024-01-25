#include <stdio.h>
#include <stdint.h>
#include <string.h>

int hash(char *str, int size) {
  uint8_t hash = 0;
  for (int i = 0; i < size; i++) {
	printf("%d += %c\n", hash, str[i]);
	hash += str[i];
	printf("%d *= 17 (%d)\n", hash, (int) hash * 17);
	hash *= 17;
	printf("%d %= 256\n", hash);
	hash %= 256;
	printf("%d\n", hash);
  }
  return hash;
}

int main() {
	char *str = "HASH";
	printf("%d\n", hash(str, strlen(str)));
	return 0;
}
