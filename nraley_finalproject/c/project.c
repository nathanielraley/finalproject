#include"header.h"
#include<stdio.h>
#include<stdlib.h>
#include<string.h>



int main(int argc, char* argv[]){

float a,alpha,tempstr;
int i,j,x,y,N,c,k,l,t,freq;
int size_x,size_y,num_timesteps,maxsize,totalpoints,holdstr;
char input[3000],output[1000],buff[2000],readin[2000],xstr[20],ystr[20];


if(argc>=3){
freq=atoi(argv[2]);
//output=argv[3];
}
else{
fprintf(stderr,"Error: please run the program again with 3 arguments (input filename, output frequency, output filename");
return 1;
}

FILE *fptr1 = fopen(argv[1],"r");
if(fptr1==NULL)
  printf("Error opening file\n");

N=(readlines(argv[1]))-1;
printf("heatpoints in file = %d\n",N);

fscanf(fptr1,"%d%d%f%d",&size_x,&size_y,&alpha,&num_timesteps);


printf("first line: x = %d, y = %d, alpha= %f, timesteps = %d\n",size_x,size_y,alpha,num_timesteps);

if(size_x>size_y)
   maxsize=size_x;
else
   maxsize=size_y;

totalpoints=size_x*size_y;

float *** heatmat= (float ***)malloc(size_x*sizeof(float**));
for (i=0;i<size_x;i++){
heatmat[i]=(float **) malloc(size_y*sizeof(float *));
for (j=0;j<size_y;j++){
heatmat[i][j]=(float *)malloc(num_timesteps*sizeof(int));
}}

for(t=0;t<num_timesteps;t++){
for(i=0;i<size_x;i++){
for(j=0;j<size_y;j++){
heatmat[i][j][t]=0;
}}}


typedef struct {
int x;
int y;
float temp;
int hold;
} heatpoints;

heatpoints heats[size_x+10][size_y+10][num_timesteps];

for(k=0;k<N;k++){
 fscanf(fptr1,"%s%s%f%d",xstr,ystr,&tempstr,&holdstr);
  if(strcmp(xstr,"*")==0 && strcmp(ystr,"*")!=0){
   for(l=0;l<size_x;l++){
heats[l][atoi(ystr)][0].temp=tempstr;
heats[l][atoi(ystr)][0].hold=holdstr;
heatmat[l][atoi(ystr)][0]=tempstr;
//printf("heats[%d][%d].temp=%f\n heats[%d][%d].hold=%d\n",l,atoi(ystr),heats[l][atoi(ystr)][0].temp,l,atoi(ystr),heats[l][atoi(ystr)][0].hold);
}}

else if(strcmp(ystr,"*")==0 && strcmp(xstr,"*")!=0){
for(l=0;l<size_y;l++){
heats[atoi(xstr)][l][0].temp=tempstr;
heatmat[atoi(xstr)][l][0]=tempstr;
heats[atoi(ystr)][l][0].hold=holdstr;
//printf("heats[%d][%d].temp=%f\n heats[%d][%d].hold=%d\n",atoi(xstr),l,heats[atoi(xstr)][l][0].temp,atoi(xstr),l,heats[atoi(xstr)][l][0].hold);
}}

else if(strcmp(ystr,"*")==0 && strcmp(xstr,"*")==0){
for(i=0;i<size_x;i++){
for(j=0;j<size_y;j++){
heats[i][j][0].temp=tempstr;
heatmat[i][j][0]=tempstr;
heats[i][j][0].hold=holdstr;
//printf("heats[%d][%d].temp=%f\n heats[%d][%d].hold=%d\n",i,j,heats[i][j][0].temp,i,j,heats[i][j][0].hold);
}}}

else{
i=atoi(xstr);
j=atoi(ystr);
heats[i][j][0].temp=tempstr;
heats[i][j][0].hold=holdstr;
heatmat[i][j][0]=tempstr;
//printf("heats[%d][%d].temp=%f\n heats[%d][%d].hold=%d\n",i,j,heats[i][j][0].temp,i,j,heats[i][j][0].hold);
}
}
for(t=0;t<num_timesteps;t++){
for(i=0;i<size_x;i++){
for(j=0;j<size_y;j++){
//printf("heatmat[%d][%d][%d]=%f\n",i,j,t,heatmat[i][j][t]);
}}}


fclose(fptr1);


for(t=1;t<num_timesteps;t++){
   for(i=0;i<size_x;i++){
      for(j=0;j<size_y;j++){
if(heats[i][j][0].hold==0){
       if(i==0 && j==0)
      heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i][j+1][t-1]+heatmat[i+1][j][t-1]+heatmat[i+1][j+1][t-1])-(3*heatmat[i][j][t-1])));
     else if(i==0 && j==(size_y-1))
	heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i][j-1][t-1]+heatmat[i+1][j-1][t-1]+heatmat[i+1][j][t-1])-(3*heatmat[i][j][t-1])));
    else if(i==(size_x-1) && j==0)
        heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i-1][j][t-1]+heatmat[i-1][j+1][t-1]+heatmat[i][j+1][t-1])-(3*heatmat[i][j][t-1])));
    else if(i==(size_x-1) && j==(size_y-1))
        heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i-1][j-1][t-1]+heatmat[i-1][j][t-1]+heatmat[i][j-1][t-1])-(3*heatmat[i][j][t-1])));
	else if(i==0 && (j!=0 && j!=(size_y-1)))
	heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i][j-1][t-1]+heatmat[i+1][j-1][t-1]+heatmat[i+1][j+1][t-1]+heatmat[i+1][j][t-1]+heatmat[i][j+1][t-1])-(5*heatmat[i][j][t-1])));
	else if(j==0 && (i!=0 && i!=(size_x-1)))
	 heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i-1][j][t-1]+heatmat[i+1][j][t-1]+heatmat[i-1][j+1][t-1]+heatmat[i][j+1][t-1]+heatmat[i+1][j+1][t-1])-(5*heatmat[i][j][t-1])));
	else if(j==(size_y-1) && (i!=0 && i!=(size_x-1)))
	heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i][j-1][t-1]+heatmat[i-1][j-1][t-1]+heatmat[i-1][j][t-1]+heatmat[i+1][j][t-1]+heatmat[i+1][j-1][t-1])-(5*heatmat[i][j][t-1])));
	else if(i==(size_x-1) && (j!=0 && j!=(size_y-1)))
	heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i-1][j][t-1]+heatmat[i][j+1][t-1]+heatmat[i-1][j+1][t-1]+heatmat[i-1][j-1][t-1]+heatmat[i][j-1][t-1])-(5*heatmat[i][j][t-1])));
	else
      heatmat[i][j][t]=heatmat[i][j][t-1]+(alpha*((heatmat[i][j+1][t-1]+heatmat[i][j-1][t-1]+heatmat[i-1][j][t-1]+heatmat[i+1][j][t-1]+heatmat[i+1][j+1][t-1]+heatmat[i-1][j+1][t-1]+heatmat[i-1][j-1][t-1]+heatmat[i+1][j-1][t-1])-(8*heatmat[i][j][t-1])));
}
else if(heats[i][j][0].hold==1)
heatmat[i][j][t]=heatmat[i][j][t-1];
}
}

/*
for(i=0;i<size_x;i++)
for(j=0;j<size_y;j++){
if(heats[i][j].hold==0)
heats[i][j].temp=0;
}
*/


}

/*
for(t=0;t<num_timesteps;t=t+freq)
for(i=0;i<size_x;i++)
for(j=0;j<size_y;j++)
printf("heatmat[%d][%d][%d]=%f\n",i,j,t,heatmat[i][j][t]);
*/
outfile(argv[3],heatmat,freq,size_x,size_y,num_timesteps);



if(argv[4]!=NULL){
FILE * pipe = popen ("gnuplot -persistent", "w");
//fprintf(pipe,"n= '`awk 'END {print NR}' < %s '",argv[3]);


for(i=1;i<num_timesteps;i++)
//fprintf(pipe,"plot '%s' every:%d u 1:2:4 with image\n",argv[3],i);
fprintf(pipe,"do for [i=1:%d] {\n plot '%s' every:1  u 1:2:4 with image\n i=i+1}\n",(num_timesteps/freq),argv[3]);
//fprintf(pipe,"plot '%s' u 1:2:4 with image\n",argv[3]);
close(pipe);
}


}



