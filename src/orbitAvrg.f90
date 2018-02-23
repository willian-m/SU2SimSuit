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
   integer :: p2,p4,p6
   integer, dimension(6) :: pnts
   complex*16 :: observable,error
end type

integer :: numOrbits,nx,ny,nz,nt,reclen
type(orbit),allocatable,dimension(:) :: orbits
call readParameters
call parametersCheck

numOrbits=nx*(1+nx)*(2+nx)/6

allocate(orbits(numOrbits))
inquire(iolength=reclen) orbits(1)%observable
open(unit=1,file='StatisticalAverage.dat',form='unformatted',access='direct',recl=reclen)
open(unit=2,file='StatisticalErrors.dat',form='unformatted',access='direct',recl=reclen)

call computeOrbits

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
   open(unit=2,file='StatisticalErrors.dat',form='unformatted',access='direct',recl=reclen)
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
      outIndex = outx(si) + (outy(si)-1)*nx + (outz(si)-1)*nx*ny
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
