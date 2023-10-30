int g() {
	int y;
	y = 10;
	return y * 2;
}

int f() {
	int x;
	x = 10;
	return 2*x + g(x);
}

int main() {
	int x;
	x = 5;
	int y;
	y = 2*x + 10 * 5 + 2;
	int z;
	z = f();
	return 0;
}
