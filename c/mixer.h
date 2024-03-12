#ifndef MIXER_H_
#define MIXER_H_

typedef struct {
    unsigned char r;
    unsigned char g;
    unsigned char b;
} tTuple;

tTuple mix(tTuple r, tTuple y, tTuple b);

#endif