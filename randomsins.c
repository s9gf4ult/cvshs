
/* #define _USE_MATH_DEFINES */

#include <stdlib.h>
#include <math.h>

#define COMP_COUNT 1000
#define SINUS_LENGTH 4000


struct Comp {
  double phase;
  double period;
  double amplitude;
};

static double randomDouble(double from, double to) {
  double d = (double)rand() / RAND_MAX;
  return from + (d * (to - from));
}

static void randomComp(struct Comp *comp) {
  double period = randomDouble(2, 500);
  double amplitude = randomDouble(0.1, 2);
  double phase = randomDouble(0, 2 * period);
  comp->phase = phase;
  comp->period = period;
  comp->amplitude = amplitude;
}
  

int main(int argc, char **argv) {
  int n, i, j;
  struct Comp *comps[5];
  double *sins[5];
  double volatile *results[4];
  for (n=0; n < 5; ++n) {
    comps[n] = calloc(COMP_COUNT, sizeof(struct Comp));
    for (i=0; i < COMP_COUNT; ++i) {
      randomComp(&comps[n][i]);
    }
  }

  for (n=0; n < 5; ++n) {
    sins[n] = calloc(SINUS_LENGTH, sizeof(double));
    for (i=0; i < SINUS_LENGTH; ++i) {
      double x = (double)i;
      double y = 0;
      for (j=0; j < COMP_COUNT; ++j) {
        struct Comp *c = &comps[n][j];
        y += c->amplitude * sin((x / c->period * 2 * M_PI) + c->phase);
      }
      sins[n][i] = y;
    }
  }

  for (n=0; n < 4; ++n) {
    results[n] = calloc(SINUS_LENGTH, sizeof(double));
    for (i=0; i < SINUS_LENGTH; ++i) {
      results[n][i] = sins[n][i] + sins[4][i];
    }
  }
    
  for (n=0; n < 5; ++n) {
    free(comps[n]);
    free(sins[n]);
  }
  for (n=0; n < 4; ++n) {
    free(results[n]);
  }
}
