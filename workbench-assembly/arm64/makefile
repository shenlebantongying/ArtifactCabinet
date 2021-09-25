hello: hello.o
	ld -o hello hello.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64

hello.o: hello.S
	as -o hello.o hello.S
