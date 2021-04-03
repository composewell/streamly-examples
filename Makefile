CFLAGS += -std=gnu11 -Wall -Wextra -pedantic -march=native -Ofast

all: examples/WordCount

examples/WordCount: examples/WordCount.c
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f examples/WordCount
