CFLAGS += -Wall -Wextra -pedantic -march=native -Ofast

EXAMPLES = examples/WordCount examples/ListDir

all: $(EXAMPLES)

examples/%: examples/%.c
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(EXAMPLES)
