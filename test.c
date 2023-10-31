int g(int x) {
	return 10 * x;
}

void print(int a, int b, int c) {
	print_int(a,b,c);
}

int f() {
	int y;
	y = 0;
	y = g(10) * 10 + 4;
	int x;
	x = y;
	print_int(g(10) * 10 + 4, y, x);
	return x;
}

int main() {
	int x;
	x = f();
	if (x == 5)
		print_int(10);
	else
		print_int(-10);

	return 0;
}
