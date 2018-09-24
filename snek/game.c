#include "game.h"

struct snake *snake_new(size_t maxsize) {
  struct snake *res =
      calloc(1, sizeof(struct snake)); /* zero-init head and tail */
  res->positions = malloc(maxsize * sizeof(struct pos));
  res->maxsize = maxsize;

  return res;
}

void snake_delete(struct snake *snake) {
  free(snake->positions);
  free(snake);
}

struct game *game_new(size_t height, size_t width) {
  struct game *res = malloc(sizeof(struct game));
  res->height = height;
  res->width = width;
  res->snake = snake_new(height * width);
  res->board = calloc(height * width, sizeof(char));

  return res;
}
void game_delete(struct game *game) {
  snake_delete(game->snake);
  free(game->board);
  free(game);
}

void snake_add_head(struct snake *snake, struct pos *pos) {
  if (snake->head_idx + 1 == snake->maxsize) {
    snake->head_idx = 0;
  } else {
    snake->head_idx++;
  }
  snake->positions[snake->head_idx] = *pos;
}

void snake_shorten_tail(struct snake *snake) {
  if (snake->tail_idx + 1 == snake->maxsize) {
    snake->tail_idx = 0;
  } else {
    snake->tail_idx++;
  }
}

size_t snake_length(struct snake *snake) {
  if (snake->head_idx >= snake->tail_idx) {
    return snake->head_idx - snake->tail_idx + 1;
  } else {
    return snake->head_idx - snake->tail_idx + 1 + snake->maxsize;
  }
};

void snake_start(struct snake *snake, struct pos *pos) {
  snake->head_idx = snake->tail_idx = 0;
  snake->positions[0] = *pos;
}

struct pos *snake_get_head(struct snake *snake) {
  return snake->positions + snake->head_idx;
}
struct pos *snake_get_tail(struct snake *snake) {
  return snake->positions + snake->tail_idx;
}
void pos_rng(struct pos *pos, size_t height, size_t width) {
  pos->y = rand() % height;
  pos->x = rand() % width;
}

char *game_at(struct game *game, struct pos *pos) {
  return game->board + pos->y * game->height + pos->x;
}
int game_oob(struct game *game, struct pos *pos) {
  return (pos->x < 0) || (pos->y < 0) || (pos->x >= game->width) ||
         (pos->y >= game->height);
}
void game_reset(struct game *game) {
  memset(game->board, 0, game->height * game->width * sizeof(*game->board));
  struct pos pos;
  game->last_dir = TURN_RIGHT;
  pos_rng(&pos, game->height, game->width*2/3);
  snake_start(game->snake, &pos);
  *game_at(game, snake_get_head(game->snake)) |= BOARD_SNAKE;

  game_rng_food(game);
}
void game_rng_food(struct game *game) {
  struct pos food;
  do {
    pos_rng(&food, game->height, game->width);
  } while (*game_at(game, &food) != BOARD_NOTHING);
  *game_at(game, &food) |= BOARD_FOOD;
}

/* NOP if turn is not an actual turn command. */
void make_turn(struct game *game, int turn) {
  int dir = game->last_dir;
  switch (turn) {
  case TURN_LEFT:
    if (dir != TURN_RIGHT) {
      dir = TURN_LEFT;
    }
    break;
  case TURN_RIGHT:
    if (dir != TURN_LEFT) {
      dir = TURN_RIGHT;
    }
    break;
  case TURN_UP:
    if (dir != TURN_DOWN) {
      dir = TURN_UP;
    }
    break;
  case TURN_DOWN:
    if (dir != TURN_UP) {
      dir = TURN_DOWN;
    }
    break;
  }
  game->last_dir = dir;
}

void add_turn(struct pos *pos, int turn) {
  switch (turn) {
  case TURN_LEFT:
    pos->x--;
    return;
  case TURN_RIGHT:
    pos->x++;
    return;
  case TURN_DOWN:
    pos->y++;
    return;
  case TURN_UP:
    pos->y--;
    return;
  }
}

int update(struct game *game, int command) {
  struct pos next_pos = *snake_get_head(game->snake);
  make_turn(game, command);
  add_turn(&next_pos, game->last_dir);
  if (game_oob(game, &next_pos)) {
    return -snake_length(game->snake);
  } else {
    char v = *game_at(game, &next_pos);
    if (v & BOARD_SNAKE) {
      return -snake_length(game->snake);
    } else {
      *game_at(game, &next_pos) |= BOARD_SNAKE;
      if (v & BOARD_FOOD){
        /* Just ate. */
        game_rng_food(game);
      }
      snake_add_head(game->snake, &next_pos);
      struct pos tail_pos = *snake_get_tail(game->snake);
      if (*game_at(game, &tail_pos) & BOARD_FOOD) {
        *game_at(game, &tail_pos) &= ~BOARD_FOOD;
      } else {
        *game_at(game, &tail_pos) &= ~BOARD_SNAKE;
        snake_shorten_tail(game->snake);
      }
    }
  }
  return 1; /* snake_length(game->snake); */
}
