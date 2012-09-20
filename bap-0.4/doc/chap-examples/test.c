int g(int y) {
  if (y == 42) { return 42; } else { return -1; }
}

int main(char **argv, int argc) {
  return g(42);
}
