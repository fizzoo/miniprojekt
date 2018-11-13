LIBS = ncurses
CXX = clang++
CFLAGS += $(shell pkg-config --cflags $(LIBS))
CXXFLAGS += -std=c++17
LDFLAGS += $(shell pkg-config --libs $(LIBS))

all: statusy

install: statusy
	cp statusy /usr/local/bin/statusy
	chmod a+X /usr/local/bin/statusy

statusy: statusy.cc
