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
	int x = f();
	print_int(x);
	return 0;
}
