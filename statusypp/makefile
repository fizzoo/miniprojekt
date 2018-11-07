LIBS = ncurses
CFLAGS += $(shell pkg-config --cflags $(LIBS))
CXXFLAGS += -std=c++17
LDFLAGS += $(shell pkg-config --libs $(LIBS))

all: statusy


statusy: statusy.cc
