CC = clang
CFLAGS = -Wall -Wextra -O2
SRC = src/main.c
OBJ = $(SRC:.c=.o)

OS := $(shell uname -s)
ifeq ($(OS), Linux)
    TARGET = cinder
    LDFLAGS = -lpthread
else ifeq ($(OS), Darwin)
    TARGET = cinder
    LDFLAGS = -lpthread
else
    TARGET = cinder.exe
    LDFLAGS = -static -D_WIN32
endif

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJ)

.PHONY: all clean
