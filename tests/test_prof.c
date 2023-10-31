int fib(int n) {
  if (n <= 1)
       return n;
  return  fib(n - 1) + fib(n - 2);
}

int syr(int n) {
  print_int(n);
  if (n == 1)
    return 0;
  if (n % 2)
    return syr(3 * n + 1);
  return syr(n / 2);
}

int main() {
  int v;
  v = fib(10);
  return syr(v);
}
