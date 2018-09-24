#ifndef GAME_H
#define GAME_H

#include <stdlib.h>
#include <string.h>

enum boardstatus {
  BOARD_NOTHING = 0,
  BOARD_SNAKE = 1,
  BOARD_FOOD = 2,
  BOARD_DEATH = 4
};

enum command { TURN_LEFT = 1, TURN_UP, TURN_DOWN, TURN_RIGHT };

struct pos {
  size_t x, y;
};

/* Single cycling array with where we just increase the tail index
   instead of doing deletions */
struct snake {
  size_t head_idx, tail_idx, maxsize;
  struct pos *positions;
};

struct game {
  size_t height, width;
  int last_dir;
  char *board;
  struct snake *snake;
};

struct snake *snake_new(size_t);
void snake_add_head(struct snake *, struct pos *);
void snake_shorten_tail(struct snake *);
struct pos *snake_get_head(struct snake *);
struct pos *snake_get_tail(struct snake *);
void snake_delete(struct snake *);
size_t snake_length(struct snake *);
void snake_start(struct snake *, struct pos *);
void pos_rng(struct pos *, size_t, size_t);

struct game *game_new(size_t, size_t);
void game_delete(struct game *);
char *game_at(struct game *, struct pos *);
int game_oob(struct game *, struct pos *);
void game_rng_food(struct game *);

/* Also start */
void game_reset(struct game *);
void make_turn(struct game *, int);
int update(struct game *);

#endif
