
program project
use subroutines
implicit none

real, allocatable::array(:,:,:)
real::alpha
character(len=15)::input,temp1,temp2,temp3,output
integer::freq,closestatus,i,j,x,y,t,numtimes,sizex,sizey


call getarg(1,temp1)
call getarg(2,temp2)
call getarg(3,temp3)

if(iargc()==0 .OR. iargc()>3) then
print*,"Error: program requires command line argument"
else
read(temp1,*)input
read(temp2,*)freq
read(temp3,*)output
end if

print*,input


call readin(input)


numtimes=size(heatmat,dim=3)
sizey=size(heatmat,dim=2)
sizex=size(heatmat,dim=1)
alpha=heats(1,1,1)%alpha


do t=1,numtimes-1
do x=1,sizex
do y=1,sizey

if(heats(x,y,1)%hold==0) then
if (x==1 .AND. y==1) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x,y+1,t)+heatmat(x+1,y,t)+heatmat(x+1,y+1,t))-(3*heatmat(x,y,t))))

else if(x==1 .AND. y==sizey) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x,y-1,t)+heatmat(x+1,y-1,t)+heatmat(x+1,y,t))-(3*heatmat(x,y,t))))
else if(x==sizex .AND. y==0) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x-1,y,t)+heatmat(x-1,y+1,t)+heatmat(x,y+1,t))-(3*heatmat(x,y,t))))
else if(x==sizex .AND. y==sizey) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x-1,y-1,t)+heatmat(x-1,y,t)+heatmat(x,y-1,t))-(3*heatmat(x,y,t))))

elseif(x==0 .AND. (y/=0 .AND. y/=sizey)) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x,y-1,t)+heatmat(x+1,y-1,t)+heatmat(x+1,y+1,t)+heatmat(x+1,y,t) &
+heatmat(x,y+1,t))-(5*heatmat(x,y,t))))
elseif(y==0 .AND. (x/=0 .AND. x/=sizex)) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x-1,y,t)+heatmat(x+1,y,t)+heatmat(x-1,y+1,t)+heatmat(x,y+1,t) &
+heatmat(x+1,y+1,t))-(5*heatmat(x,y,t))))
elseif(y==sizey .AND. (x/=0 .AND. x/=sizex)) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x,y-1,t)+heatmat(x-1,y-1,t)+heatmat(x-1,y,t)+heatmat(x+1,y,t) &
+heatmat(x+1,y-1,t))-(5*heatmat(x,y,t))))
elseif(x==sizex .AND. (y/=0 .AND. y/=sizey)) then
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x-1,y-1,t)+heatmat(x,y+1,t)+heatmat(x-1,y+1,t)+heatmat(x-1,y-1,t) &
+heatmat(x,y-1,t))-(5*heatmat(x,y,t))))
else
heatmat(x,y,t+1)=heatmat(x,y,t)+(alpha*((heatmat(x,y+1,t)+heatmat(x,y-1,t)+heatmat(x-1,y,t)+heatmat(x+1,y,t) & 
+heatmat(x+1,y+1,t)+heatmat(x-1,y+1,t)+heatmat(x-1,y-1,t)+heatmat(x+1,y-1,t))-(8*heatmat(x,y,t))))
endif

else if(heats(x,y,1)%hold==1) then
heatmat(x,y,t+1)=heatmat(x,y,t)

endif
enddo
enddo
enddo

call printout(output,freq,sizex,sizey,numtimes)









deallocate(heatmat,stat=closestatus)
if (closestatus/=0) then
print*,"Error deallocating arrays"
end if
deallocate(heats,stat=closestatus)
if (closestatus/=0) then
print*,"Error deallocating arrays"
end if



end program project


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

