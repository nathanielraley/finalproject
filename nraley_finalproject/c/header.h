#ifndef HEADER_INCLUDED
#define HEADER_INCLUDED
int readlines(char *filename);
void readfile(char *filename);
void outfile(char *filename, float ***heatmat, int freq, int size_x, int size_y, int num_timesteps);
#endif

