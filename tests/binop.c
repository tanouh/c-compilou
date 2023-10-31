int g(int x) {
	int y;
	y = 10;
	return y * x;
}

void print(int a) {
	print_int(a);
}

int f() {
	int x;
	x = 10;
	return 2*x + g(print(x));
}

int true() {
	return 1;
}

int false () {
		return 0;
}

int _42() {
	return 42;
}

int _100() {
	return 100;
}

int main() {
	print(true() || false());
	print(_42() < _100());
	print(_42() <= _100());
	print(_42() > _100());
	print(_42() >= _100());
	print(_42() != _100());
	print(_100() == _100());
	print(true() && false());
	print(true() && true());
	print(false() || false());
	print(_100() % _42());
	return 0;
}
