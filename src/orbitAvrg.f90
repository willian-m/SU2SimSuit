!Willian Matioli Serenone
!Instituto de Fisica de Sao Carlos

!Now we go to the second step: We average over data on the same orbits
!Points in same orbit are those with the same p^[2], p^[4] and p^[6]
!p^[n] = p_x^n + p_y^n + p_z^n
!Time direction is not included because usually they have a different size
!than space directions, breaking the lattice symmetry that allow us to do this

!We will assume that exists a file StatisticalAverage.dat and StatisticalError.dat!If that is not the case, you should run the Statistical Averager before

program orbitAvrg
implicit none

type orbit
   integer*8 :: p2,p4,p6 !If the lattice is large, e.g. ns=2**6 or ns=2**7
                         !Then we will need long integers. These numbers contains
                         !up to 19 digits
   integer, dimension(6) :: pnts
   complex*16,allocatable,dimension(:) :: observable,error
end type
complex*16 :: aux
integer :: numOrbits,nx,ny,nz,nt,reclen,o,pnt,t
type(orbit),allocatable,dimension(:) :: orbits

print *, "Reading parameters.."
call readParameters
inquire(iolength=reclen) aux
call parametersCheck

numOrbits=nx*(1+nx)*(2+nx)/6

print *, "Allocating arrays..."
allocate(orbits(numOrbits))
do o=1,numOrbits
   allocate(orbits(o)%observable(nt))
   allocate(orbits(o)%error(nt))
end do

print *, "There are", numOrbits, "orbits"
print *, "Computing points in each orbit..."
call computeOrbits



print *, "Averaging over orbits..."
do o=1,numOrbits
   orbits(o)%observable = 0.d0
   orbits(o)%error = 0.d0
   do t=1,nt
      do pnt=1,6
!         if (orbits(o)%pnts(pnt)+t*nx**3 .gt. nx*ny*nz*nt) then
!            print *, "I jumped outside the lattice. Exiting."
!            print *, "Lattice size:",nx*ny*nz*nt
!            print *, "t=",t
!            print *, "pnt=",orbits(o)%pnts(pnt)
!            call exit(-1)
!         end if

         read(1,rec=orbits(o)%pnts(pnt)+(t-1)*nx**3) aux
         orbits(o)%observable(t) = orbits(o)%observable(t) + aux
         read(2,rec=orbits(o)%pnts(pnt)+(t-1)*nx**3) aux
         orbits(o)%error(t) = orbits(o)%error(t) + aux
      end do
      orbits(o)%observable=orbits(o)%observable(t)/6
      orbits(o)%error=orbits(o)%error(t)/6
!      print *, o*t, "averages of", nt*numOrbits, "completed." 
   end do
end do

close(1)
close(2)

print *,"Writting data to file..."
!Now that we averaged over orbits, we write the results *in plain text*
!Notice: We will throw away the imaginary part. Here is the reason why 
!we do this.
!We ommited the negative quadrants because we know that they are the 
!complex conjugate of the positive quadrants. This does not mean they
!should be ommited from the averaging procedure. Thus, we will have sum
!of complex numbers with their complex conjugate and then we can safely
!take only the real part of the numbers.

open(unit=10,file="orbitAveraged.out")
   write(10,'(A5,17X,3(A3,19X),A7,19X,A5)') '# p^2','p^4','p^6','p_t','Average','Error'
do o=1,numOrbits
   do t=1,nt
      write(10,'(4(I19.19,3X),2(ES23.15E3,3X))') orbits(o)%p2,orbits(o)%p4,orbits(o)%p6,t,dble(orbits(o)%observable(t)), dble(orbits(o)%error(t))
   end do
end do
close(10)

deallocate(orbits)

print *, "Done! All tasks completed."
print *, "It is logical to me to exit now. Live Long and Prosper \\//_"

contains

   subroutine parametersCheck
   logical :: statAverExist, statErrorExist
   if (nx .ne. ny) then
      print *, "H(3) group broken. My computations only make sense if nx = ny = nz"
      call EXIT(-2)
   end if

   if (nx .ne. nz) then
      print *, "H(3) group broken. My computations only make sense if nx = ny = nz"  
      call EXIT(-2)
   end if

   if (nz .ne. ny) then
      print *, "H(3) group broken. My computations only make sense if nx = ny = nz"  
      call EXIT(-2)
   end if

   open(unit=1,file='StatisticalAverage.dat',form='unformatted',access='direct',recl=reclen)
   open(unit=2,file='StatisticalError.dat',form='unformatted',access='direct',recl=reclen)
   inquire(unit=1,exist=statAverExist)
   inquire(unit=2,exist=statErrorExist)

   if (.not. statErrorExist) then
      print *, "I could not find the file containing the statistical error."
      call EXIT(-3)
   end if

   if (.not. statAverExist) then
      print *, "I could not find the file containing the statistical averages."
      call EXIT(-3)
   end if

   end subroutine

   subroutine readParameters
      character(len=50) :: argNx, argNy, argNz, argNt
      integer, parameter :: minNumberParameters = 4 !Change this if input changes

 if (COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
         print*, "It is mandatory to pass at least 4 parameters as input in the format:"
         print*, "nx ny nz nt "
         print*, "You should have two files in the running folder and named StatisticalAverage.dat and StaticalError.dat"
         print*, "If you do not have them, you should run the statistical averager program before."
         call EXIT(1)
      else
         call GET_COMMAND_ARGUMENT(1,argNx)
         call GET_COMMAND_ARGUMENT(2,argNy)
         call GET_COMMAND_ARGUMENT(3,argNz)
         call GET_COMMAND_ARGUMENT(4,argNt)
         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt

      end if
   end subroutine

   subroutine pntsInOrbit(mx,my,mz,outIndex)
   implicit none
   integer,intent(in) :: mx,my,mz
   integer,dimension(6) :: outx,outy,outz
   integer,dimension(6),intent(out) :: outIndex
   integer :: si

!  The 6 permutations on the positive quadrant
!  We do not consider the negative quadrants because
!  The input data are a FFT of real data, thus we did
!  not stored the negative quadrants, since they are just the
!  complex conjugate of the positive quadrants
   outx(1)=mx
   outy(1)=my
   outz(1)=mz

   outx(2)=mx
   outy(2)=mz
   outz(2)=my

   outx(3)=my
   outy(3)=mx
   outz(3)=mz

   outx(4)=my
   outy(4)=mz
   outz(4)=mx

   outx(5)=mz
   outy(5)=mx
   outz(5)=my

   outx(6)=mz
   outy(6)=my
   outz(6)=mx

   do si=1,6
      outIndex(si) = outx(si) + (outy(si)-1)*nx + (outz(si)-1)*nx*ny
   end do

   end subroutine

   !Fill the orbits vector
   subroutine computeOrbits
   integer :: x,y,z,i

   i=1
   do x=1,nx
      do y=1,x
         do z=1,y
            orbits(i)%p2 = x**2 + y**2 + z**2
            orbits(i)%p4 = x**4 + y**4 + z**4
            orbits(i)%p6 = x**6 + y**6 + z**6
            call pntsInOrbit(x,y,z,orbits(i)%pnts)
            i=i+1 
         end do
      end do
   end do
   
   end subroutine

end program
