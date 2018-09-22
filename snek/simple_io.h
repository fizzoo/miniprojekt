#ifndef SIMPLE_IO_H
#define SIMPLE_IO_H

#include "game.h"
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <sys/select.h>

/* Using atexit for cleanup. */
void io_setup(void);

void print_game(struct game *);

/* Return a command if there is any input queued, or 0 otherwise. */
int get_input(void);


#endif
