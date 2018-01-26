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
character(len=50) :: latticeFile = ''
double precision, allocatable, dimension(:,:) :: T0i
integer :: s,seed2
!Load parameters (lattice size and lattice file name)
call readArgs() 
write(6,*) s

!Allocates lattice and loads file
call initiate_auxTables()
call load_lattice(latticeFile)

!Computes T0i over the entire lattice
allocate(T0i(3,0:nx*ny*nz*nt-1))
call CalcT0i(T0i)


contains
   subroutine readArgs()
      integer, parameter :: minNumberParameters = 5
      character(len=50) :: argNx,argNy,argNz,argNt,argSourcePos
      real :: r
      if(COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
         print*, "It is mandatory to pass 5 arguments in the format"
         print*, "nx ny nz nt path/to/Lattice/File [Source Position Index]"
         print*, "Exiting now"
         write(6,*) COMMAND_ARGUMENT_COUNT()
         call EXIT(1)
      else
      
         call GET_COMMAND_ARGUMENT(1,argNx)
         call GET_COMMAND_ARGUMENT(2,argNy)
         call GET_COMMAND_ARGUMENT(3,argNz)
         call GET_COMMAND_ARGUMENT(4,argNt)
         call GET_COMMAND_ARGUMENT(5,latticeFile)
         call GET_COMMAND_ARGUMENT(6,argSourcePos)

         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt
         if (argSourcePos .ne. '') then
            read(argSourcePos,*) s
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


