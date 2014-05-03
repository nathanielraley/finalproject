#include"header.h"
#include<stdio.h>

int readlines(char *filename)
{
FILE *fp = fopen(filename,"r");
        int c, nl;
        nl = 0;
        while (!feof(fp)){
                c = fgetc(fp);
        if (c=='\n'){
                nl++;}
}
fclose(fp);
return nl;
}


void outfile(char *filename, float ***heatmat, int freq, int size_x, int size_y, int num_timesteps)
{

FILE *fptr1 = fopen(filename,"w");
if(fptr1==NULL)
  printf("Error opening file\n");

int i, j, t;

for(t=0;t<num_timesteps;t=t+freq){
for(i=0;i<size_x;i++){
for(j=0;j<size_y;j++){
fprintf(fptr1,"%d	%d	%d	%f\n",i,j,t,heatmat[i][j][t]);
}
}
}
fclose(fptr1);
}


