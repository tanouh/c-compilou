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
	print_int(true() || false());
	print_int(_42() < _100());
	print_int(_42() <= _100());
	print_int(_42() > _100());
	print_int(_42() >= _100());
	print_int(_42() != _100());
	print_int(_100() == _100());
	print_int(true() && false());
	print_int(true() && true());
	print_int(false() || false());
	print_int(_100() % _42());
	return 0;
}
