LIBS = ncurses
CFLAGS += $(shell pkg-config --cflags $(LIBS))
LDFLAGS += $(shell pkg-config --libs $(LIBS))

all: snek


snek: snek.c game.o simple_io.o

clean:
	rm -f snek game.o simple_io.o
