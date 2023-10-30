int g(int x) {
	int y;
	y = 10;
	return y * x;
}

void print(int a, int b, int c, int d, int e, int f, int g, int h, int i , int j, int k) {
	print_int(a,b, c, d, e, f, g, h , i , j, k);
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
	z = 10;
	print(1,2,3,4,5,6,7, 8, 9, 10, 11);
	return 0;
}
