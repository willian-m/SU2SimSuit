!Willian Matioli Serenone
!Institution: Universidade de Sao Paulo
!                  Instituto de Fisica de Sao Carlos
!e-mail: willian.matioli@gmail.com
!######################################################################

!     PROGRAM: Monte_Carlo
!     This program is intended to generate lattice configurations using Monte Carlo
!methods. You can choose between Metropolis and Heat-Bath methods. This program receives 
!a list of strings as input. We list them bellow
!
!   -4 integers representing the lattice dimensions nx, ny, nz and nt
!   -A float corresponding to the value of beta
!   -H or C to choose between a "Hot" start or a "Cold" start
!   -A (large) integer to tell how many MC steps will be performed
!   -A number telling the intervall for which each lattice configuration will be stored. Must be different of 0
!   -Optionally, one may give the name of a lattice configuration. This enables
!one to skip the thermalization phase using as starting point a configuration previously
!thermalized.
!   ---IMPORTANT: This is not the same as continuing the previously simulation, since the
!random numbers sequence are reset.

!This is the main program. It depends directly or indirectly on the following modules
!   -lattice: We define the lattice configuration and global variables here
!   -math: Implementations of math operations, such as matrix operations
!   -physics: Computation of lattice action, plaquettes, staples etc is done here
!   -monteCarlo: Implementation of Monte Carlo methods
!   -ziggurat: Library of random number generators
!   -IOfunctions: Contains subroutines to write data to disk
!
!   Program usage: after compiled, run the executable as .\LatticeGen.run nx ny nz nt beta H/C NMC
!   The output will be in binary format in a file named lattice-nx_ny_nz_nt-C/T-T/F-NMC-beta.dat, where
!nx,ny etc will be replaced with the values of the input variables
   program gen_lat_config

   use lattice
   use MonteCarlo
   use physics
   use IOfunctions

   implicit none
   !====THESE ARE NECESSARY TO RUN A BASIC SIMULATION=====
   integer :: clock,NMC,MC_time, recInterval
   character(len=1024) :: latticeFile = ''
   !======================================================

   call readArgs(nx,ny,nz,nt,hotStart,NMC,beta,recInterval,latticeFile) !read input parameters

   if (latticeFile .eq. '') then
     call lattice_start() !initialize the lattice and fill neighbour table
   else
     call initiate_auxTables()
     call load_lattice(latticeFile)
   end if

   !Initiate ziggurat random number generator
   clock = 123
!=====UNCOMENT CALL BELLOW TO OBTAIN DIFFERENT RESULTS EACH RUN====================
   !call system_clock(count=clock)
!==================================================================================
   call zigset(clock)
   !This is for the use of the native fortran random number generator
   call random_seed(clock)

   write(6,*) "Starting simulation for beta fixed at",beta
   print*, "Lattice size is ",nx," x ",ny," x ",nz," x ",nt,"."

   !Now we perform the simulation itself
   print *, "Using Heat-Bath method"
   call heatBathMethod()
   close(10)

   print*, "Done. All tasks completed."
   print*, "Lattice configuration stored in .dat files."
   print*, "Live long and prosper \\//_"

   contains
   !I use these subroutine under a contains because they share variable names with the main
   !In fact, no new variable should ever be declared inside these subroutines. Use the main code
   !and its modules for this. They also receive no input parameters

      subroutine heatBathMethod()
         do MC_time=2,NMC
            !One MC step is performed
            call heatBathIterator()
            if (mod(MC_time,recInterval) .eq. 0) then
               call writeLattice(MC_Time)
            end if
            write(6,fmt="(a1,F6.2,a11)") char(13), real(MC_time)*100.d0/real(NMC),"% completed"
            !print *, real(MC_time)*100.d0/real(NMC), "completed." !This is just for the user know we are still working
        end do
      end subroutine

      END PROGRAM


!####################################################################################

!####################################################################################

!=====Subroutine to read input arguments
      SUBROUTINE readArgs(x,y,z,t,hotStart,NMC,beta,RecStep,latticeFile)

      implicit none

      !Output variables
      integer, intent(out) :: x,y,z,t
      integer, intent(out) :: NMC,RecStep
      logical, intent(out) :: hotStart
      character(len=50) :: latticeFile
      real*8, intent(out) :: beta
      !Internal variables
      integer, parameter :: minNumberParameters = 8 !Change this if input changes
      integer, parameter :: maxNumberParameters = 9 !Change this if input changes
      character(len=50) :: argNx,argNy,argNz,argNt,argHotStart,argNMC,argBeta,argRecStep

      if (COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
        print*, "It is mandatory to pass at least 8 parameters as input in the format:"
        print*, "nx ny nz nt beta H/C NMC recordStep [lattice/Data/FilePath.dat]"
        print*, "Exiting now"
        call EXIT(1)
      else
        call GET_COMMAND_ARGUMENT(1,argNx)
        call GET_COMMAND_ARGUMENT(2,argNy)
        call GET_COMMAND_ARGUMENT(3,argNz)
        call GET_COMMAND_ARGUMENT(4,argNt)
        call GET_COMMAND_ARGUMENT(6,argHotStart)
        call GET_COMMAND_ARGUMENT(7,argNMC)
        call GET_COMMAND_ARGUMENT(5,argBeta)
        call GET_COMMAND_ARGUMENT(8,argRecStep)
        call GET_COMMAND_ARGUMENT(9,latticeFile)
        read(argNx,*) x
        read(argNy,*) y
        read(argNz,*) z
        read(argNt,*) t
        if (argHotStart(1:1) .eq. "H") then
            hotStart = .true.
        else if (argHotStart(1:1) .eq. "C") then
            hotStart = .false.
        else
            print*,'WARNING: 6th argument must be "H" or "C"'
            print*,'=======I will proceed assuming a cold lattice start'
            hotStart = .false.
        end if

        read(argNMC,*) NMC
        read(argBeta,*) beta
        read(argRecStep,*) RecStep
      end if
      END SUBROUTINE

!=====We use this subroutine to initialise the lattice in hot/cold status and
!fill the neighbour table
      SUBROUTINE lattice_start()

      use lattice

      implicit none
      
      call initiate_auxTables()

      if (hotStart) then
        call initiate_lattice_hot()
      else
        call initiate_lattice_cold()
      end if

      END SUBROUTINE 


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
            write(6,*) seed2
            DEALLOCATE(seed)
      END SUBROUTINE
