int g(int x) {
	int y;
	y = 10;
	return y * x;
}

void print(int a, int b, int c) {
	print_int(a,b,c);
}

int f() {
	int x;
	x = 10;
	return 2*x + g(x);
}

int main() {
	int x;
	x = f();
	int y;
	y = 2 * 10 + x;
	int z;
	z = f() + f();
	print(x,y,z);
	
	return 0;
}
