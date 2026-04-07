code: main.cpp
	g++ -O2 -pipe -static -s -std=gnu++17 -o code main.cpp

.PHONY: clean
clean:
	rm -f code *.o
