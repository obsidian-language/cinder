CC = gcc
CFLAGS = -std=c17 -O2 -Wall -Wextra -Wshadow -Wundef -Wwrite-strings -Wredundant-decls -Wdisabled-optimization -Wdouble-promotion -Wmissing-declarations -Wconversion -Wstrict-overflow=2 -Wfatal-errors
LDFLAGS =

SRC = src/cinder.c
OBJ = $(SRC:.c=.o)
TARGET = cinder

INCLUDE_DIR = src/include

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -I$(INCLUDE_DIR) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)

.PHONY: all clean