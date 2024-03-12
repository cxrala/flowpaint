#include <stdio.h>
#include "mixbox.h"
#include <mixer.h>

int main() {
  unsigned char r1 =   0, g1 = 33,  b1 = 133; // blue
  unsigned char r2 = 252, g2 = 211, b2 = 0;   // yellow
  float t = 0.5;
  unsigned char r, g, b;

  mixbox_lerp(r1, g1, b1,  // first color
              r2, g2, b2,  // second color
              t,           // mixing ratio
              &r, &g, &b); // result

  printf("%d %d %d\n", r, g, b);
}

tTuple mix(tTuple r, tTuple y, tTuple g) {
    
}