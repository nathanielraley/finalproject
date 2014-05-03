module subroutines

real,dimension(:,:,:), allocatable::heatmat

type::heattype
integer::hold
real::temp,alpha
endtype heattype

type(heattype),dimension(:,:,:),allocatable::heats



CONTAINS



subroutine readin(input)


character(len=15),intent(in)::input
integer::openstatus, closestatus,sizex,sizey,numtimes,holdstr,check,xint,yint,j,l,k
real::alpha,heatstr
character::xstr,ystr

open(11,file=input,iostat=openstatus,action="read")
if(openstatus /= 0) then
print *,"Error opening file"
end if

read(11,*)sizex,sizey,alpha,numtimes

print*,sizex, sizey, alpha, numtimes

allocate(heatmat(sizex,sizey,numtimes+1))

allocate(heats(sizex,sizey,numtimes+1))


heatmat=0
heats(1,1,1)%alpha=alpha

!print*,heatmat

do
read(11,*,iostat=check)xstr,ystr,heatstr,holdstr

if(check<0) then
exit

else if(xstr=="*" .AND. ystr/="*") then
read(ystr,*)yint
yint=yint+1
do l = 1,sizex
heats(l,yint,1)%temp=heatstr
heats(l,yint,1)%hold=holdstr
heatmat(l,yint,1)=heatstr
enddo


else if(ystr=="*" .AND. xstr/="*") then
read(xstr,*)xint
xint=xint+1
do l = 1,sizey
heats(xint,l,1)%temp=heatstr
heats(xint,l,1)%hold=holdstr
heatmat(xint,l,1)=heatstr
enddo


else if(xstr=="*" .AND. ystr=="*") then
do l = 1,sizex
do k = 1,sizey
heats(l,k,1)%temp=heatstr
heats(l,k,1)%hold=holdstr
heatmat(l,k,1)=heatstr
enddo
enddo

else
read(xstr,*)xint
xint=xint+1
read(ystr,*)yint
yint=yint+1
heats(xint,yint,1)%temp=heatstr
heats(xint,yint,1)%hold=holdstr
heatmat(xint,yint,1)=heatstr

end if
end do

close(11,iostat=closestatus)
if (closestatus /= 0) then
print*, "Error closing file"
end if



end subroutine readin








subroutine printout(output,freq,sizex,sizey,numtimes)

character(len=15),intent(in)::output
integer::openstatus, closestatus,t,x,y
integer,intent(in)::freq,sizex,sizey,numtimes
real::alpha,heatstr
character::xstr,ystr

open(11,file=output,iostat=openstatus,action="write")
if(openstatus /= 0) then
print *,"Error opening file"
end if

write(11,*)"  !! x !! y !! time !! temp !!"
write(11,*)""
do t=1,numtimes
do y=1,sizey
do x=1,sizex
write(11,'(3I6,F10.5)')x,y,t,heatmat(x,y,t)
enddo
enddo
enddo

close(11,iostat=closestatus)
if (closestatus /= 0) then
print*, "Error closing file"
end if


end subroutine printout




end module subroutines


