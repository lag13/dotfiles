all: run

run: build
	./a.out

build: main.o library.o
	gcc main.o library.o

main.o: main.c
	gcc -c -Wall main.c

library.o: library.c
	gcc -c -Wall library.c
