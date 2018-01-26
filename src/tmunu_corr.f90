!This program is devoted to the comp-utation of the correlation function
! T0i(x)T0j(0) in each point for one lattice configuration.
! 0 is the source and we will ask the user to either define which point on
! the lattice correspond to it, or set it randomly. The source position will
! be encoded on the file name

program tmunu_corr

use IOfunctions
use lattice
implicit none
character(len=50) :: latticeFile = ''

!Load parameters (lattice size and lattice file name)
call readArgs(nx,ny,nz,nt) 

!Allocates lattice and loads file
call initiate_auxTables()
call load_lattice(latticeFile)

!


contains
   subroutine readArgs(x,y,z,t)
      integer, intent(out) :: x,y,z,t
      integer, parameter :: minNumberParameters = 5
      character(len=50) :: argNx,argNy,argNz,argNt
      if(COMMAND_ARGUMENT_COUNT() .ne. 4) then
         print*, "It is mandatory to pass 5 arguments in the format"
         print*, "nx ny nz nt"
         print*, "Exiting now"
         call EXIT(1)
      else

        call GET_COMMAND_ARGUMENT(1,argNx)
        call GET_COMMAND_ARGUMENT(2,argNy)
        call GET_COMMAND_ARGUMENT(3,argNz)
        call GET_COMMAND_ARGUMENT(4,argNt)
        call GET_COMMAND_ARGUMENT(5,latticeFile)

        read(argNx,*) x
        read(argNy,*) y
        read(argNz,*) z
        read(argNt,*) t

      end if
   end subroutine
end program
