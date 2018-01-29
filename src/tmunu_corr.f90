!This program is devoted to the comp-utation of the correlation function
! T0i(x)T0j(0) in each point for one lattice configuration.
! 0 is the source and we will ask the user to either define which point on
! the lattice correspond to it, or set it randomly. The source position will
! be encoded on the file name

program tmunu_corr

use IOfunctions
use lattice
use Measurements
implicit none
character(len=1024) :: latticeFile = ''
character(len=50) :: filename = ''
double precision, allocatable, dimension(:,:) :: T0i
double precision, allocatable, dimension(:,:,:) :: T0iT0j
integer :: s,seed2,x,d1,d2
!Load parameters (lattice size and lattice file name)
call readArgs() 

!Allocates lattice and loads file
call initiate_auxTables()
call load_lattice(latticeFile)

!Computes T0i over the entire lattice
allocate(T0i(3,0:nx*ny*nz*nt-1))
allocate(T0iT0j(3,3,0:nx*ny*nz*nt-1))

call CalcT0i(T0i)
do x=0,nx*ny*nz*nt-1
   do d1=1,3
      do d2=1,3
         T0iT0j(d1,d2,x) = T0i(d1,x)*T0i(d2,s)
      end do
   end do
end do


write(filename,"('source',I5.5,'.dat')") s
open(unit=10,file=trim(latticeFile)//trim(filename),form="unformatted")
write(10) T0i
close(10)

contains
   subroutine readArgs()
      integer, parameter :: minNumberParameters = 6
      character(len=50) :: argNx,argNy,argNz,argNt,argSourcePos,argBeta
      real :: r
      if(COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
         print*, "It is mandatory to pass 6 arguments in the format"
         print*, "nx ny nz nt beta path/to/Lattice/File [Source Position Index]"
         print*, "Exiting now"
         call EXIT(1)
      else
      
         call GET_COMMAND_ARGUMENT(1,argNx)
         call GET_COMMAND_ARGUMENT(2,argNy)
         call GET_COMMAND_ARGUMENT(3,argNz)
         call GET_COMMAND_ARGUMENT(4,argNt)
         call GET_COMMAND_ARGUMENT(5,argBeta)
         call GET_COMMAND_ARGUMENT(6,latticeFile)
         call GET_COMMAND_ARGUMENT(7,argSourcePos)

         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt
         read(argBeta,*) beta
         if (argSourcePos .ne. '') then
            read(argSourcePos,*) s
            if (s .gt. nx*ny*nz*nt) then
               write(6,*) "Supplied source position is greater than lattice size. Halting now."
               call EXIT(1)
            end if
         else
            call init_random_seed(seed2)
            CALL RANDOM_NUMBER(r)
            s = int(real(nx*ny*nz*nt)*r)
         end if  

      end if
   end subroutine
end program

!=====Subroutine to initialize the random seed.
!     It is available at http://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
!     Modified to initialize the ziggurat random number generator as well
      SUBROUTINE init_random_seed(seed2)
            INTEGER :: i, n, clock,seed2
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))

            CALL SYSTEM_CLOCK(COUNT=clock)

            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)
            seed2 = seed(1)
!            write(6,*) seed2
            DEALLOCATE(seed)
      END SUBROUTINE


