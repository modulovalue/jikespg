#ifndef SPACE_INCLUDED
#define SPACE_INCLUDED

struct new_state_type {
  struct reduce_header_type reduce;
  short shift_number;
  short link;
  short thread;
  short image;
};

static struct new_state_type *new_state_element;

static short *shift_image = NULL;
static short *real_shift_number = NULL;

static int *term_state_index = NULL;
static int *shift_check_index = NULL;

static int shift_domain_count;
static int num_terminal_states;
static int check_size;
static int term_check_size;
static int term_action_size;
static int shift_check_size;

#endif /* SPACE_INCLUDED */
