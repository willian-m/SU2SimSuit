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
   integer :: record_len
   contains
      
      subroutine writeLattice(sweepNum)
         integer,intent(in) :: sweepNum
         integer*8 :: i,dir
         character(len=50) :: fileName
         inquire(iolength=record_len) U(1,1)%a
         write(filename,"('links',4I3.3,'beta',F4.2,'Sweep',I9.9,'.dat')") nx,ny,nz,nt,beta,sweepNum
         open(unit=1,status='replace',file=filename,form='unformatted',access='direct',recl=record_len)
         do i=0,nx*ny*nz*nt-1
            do dir=1,8
               write(1,rec=dir+8*i) U(dir,i)%a
            end do
         end do
         close(1)
      end subroutine

      subroutine readLattice(filename)
         character(len=1024), intent(in) :: filename
         integer*8 :: i,dir

         inquire(iolength=record_len) U(1,1)%a
         open(unit=10,status='old',file=filename,form='unformatted',access='direct',recl=record_len)
         do i=0,nx*ny*nz*nt-1
            do dir=1,8
               read(10,rec=dir+8*i) U(dir,i)%a
            end do
         end do
         close(10)
      end subroutine readLattice



      end module IOfunctions
