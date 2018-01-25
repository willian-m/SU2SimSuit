!=====THE LATTICE MODULE======
!This module stores global variables and declares the lattice

!-----------------------------------------------------
!=====VARIABLES AND TYPES=====
!-----------------------------------------------------

!     -lastWrite: keeps track of when was the last time the lattice
!     configuration was recorded in a file. We index the configuration
!     by number of times that the algoritm has sweeped over the lattice.

!-----------------------------------------------------
!=====SUBROUTINES AND FUNCTIONS======
!-----------------------------------------------------
!

module IOfunctions

   use lattice
   implicit none
   
   contains
      
      subroutine writeLattice(sweepNum)
         implicit none
         integer,intent(in) :: sweepNum
         character(len=50) :: fileName
         write(filename,"('links',4I3.3,'beta',F4.2,'Sweep',I9.9,'.dat')") nx,ny,nz,nt,beta,sweepNum
         open(unit=1,status='replace',file=filename,form='unformatted')
         write(1) U 
         close(1)
      end subroutine

      subroutine readLattice(filename)
      use lattice
      character(len=50), intent(in) :: filename

      open(unit=10,status='old',file=filename,form='unformatted')
      read(10) U
      close(10)

      end subroutine readLattice



      end module IOfunctions
