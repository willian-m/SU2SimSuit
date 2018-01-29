!=====THE LATTICE MODULE======
!This module stores global variables related to the lattice
!as well functions to initialize it

!-----------------------------------------------------
!=====VARIABLES AND TYPES=====
!-----------------------------------------------------

!     -nx,ny,nz,nt: The lattice dimensions
!     -NMC: Number of Monte Carlo steps
!     -hotStart: true if we wish a hot start of the lattice. False otherwise
!     -beta: The value of the coupling beta
!
!     -U(1:nx,1:ny,1:nz,1:nt) is the gauge variable. We define it being as being of SU(2) type
!       *Easily changeable by switching its type
!     -neighTable: Table of neighbour sites
!
!     -type :: su2matrix -> A SU(2) matrix, parametrized by a(i), where a(i)*sigma(i)+a(4)*IdentityMatrix
!       !repeated index implies a sum from i=1..3 and sigma(i) is the Pauli Matrix
        !Members:
        !   -a: array of type double and dimension 4
!     -type :: su3matrix -> A SU(3) matrix, parametrized by a(i), where a(i)*lambda(i)+a(9)*IdentityMatrix
!       !repeated index implies a sum from i=1..8 and lambda(i) is the Gell-Mann Matrix
        !Members:
        !   -a: array of type double and dimension 9
!     -type :: fourDTable -> Gathers four tables that needs to be allocated. Its intend use is to store
        !the table of neighbour sites
        !Members:
        !   -x: 2D allocatable array of type integer
        !   -y: 2D allocatable array of type integer
        !   -z: 2D allocatable array of type integer
        !   -t: 2D allocatable array of type integer

!-----------------------------------------------------
!=====SUBROUTINES AND FUNCTIONS======
!-----------------------------------------------------
!
!     -initiate_lattice_cold(): allocates and initiates the lattice in a cold state
!     -initiate_lattice_hot(): allocates and initiates the lattice in a hot state
!     -initiate_neighTable(): It will initiate and fill the table of neighbour sites

      module lattice

      use mathSU2

      implicit none
      integer :: nx,ny,nz,nt
      logical :: hotStart
      double precision :: beta

      !A SU(N) matrix can be specified by using N^2 real numbers. You can generate
      !the matrix by performing the operation
      !
      !U = I*sum_{i=1}^{N^2-1} a(i)*lamda(i)+a(N^2)*identity, where lambda(i) are the
      !group generators (pauli or gell-mann matrices, for example). Also, a(i)
      !must follow the relation
      !
      !sum_{i=1}^{N^2} a(i)^2 = 1
      type :: su2matrix
        double precision,dimension(4) :: a
      end type su2matrix

      TYPE(su2matrix), allocatable, target :: U(:,:)
      !Even parameters on the direction = positive direction
      !Odd parameter on the direction = negative direction
      !U(1,i) = U_{-1}(i)
      !U(3,i) = U_{-2}(i)
      !U(5,i) = U_{-3}(i)
      !U(7,i) = U_{-4}(i)
      !U(2,i) = U_1(i)
      !U(4,i) = U_2(i)
      !U(6,i) = U_3(i)
      !U(8,i) = U_4(i)
      
      
      integer, allocatable, dimension(:,:,:,:) :: position
      !Table to translate position i,j,k,l into a single integer
      integer, allocatable, dimension(:,:) :: incrementTable !Table that tells how much I have to add to the positions
      !First entry is your current position(a number between 0 and nx*ny*nz*nt-1)
      !Last number is the direction you want to walk.
      !Positive direction: Walk forward
      !Negative direction: Walk backaward
      !Uses the same direction convention as for U
      

      contains!======================FROM HERE WE WILL DECLARE SUBROUTINES TO DECLARE THE LATTICE AND INITIALISE IT

      !=====INITIATE THE LATTICE IN A COLD STATE (ALL VECTORS WITH THE SAME VALUE)
      subroutine initiate_lattice_cold()
      implicit none
      integer :: i,j,k,l,d,x

      allocate(U(8,0:nx*ny*nz*nt-1))
      
      do l=1,nt
      do k=1,nz
      do j=1,ny
      do i=1,nx
          x = position(i,j,k,l)
          do d=2,8,2
              U(d,x)%a(1) = 0.187436259181282
              U(d,x)%a(2) = -0.770585894513425
              U(d,x)%a(3) = 0.279332775
              U(d,x)%a(4) = 0.541330055
              call hermitian_conjugate(U(d,x)%a,U(d-1,x+incrementTable(x,d))%a)
          end do
      end do
      end do
      end do
      end do

      end subroutine initiate_lattice_cold

!=====INITIATE THE LATTICE IN A HOT STATE (VECTORS RAMDOMLY DISTRIBUTED)
      subroutine initiate_lattice_hot()
      implicit none
      integer :: i,j,k,l,d,x

      allocate(U(8,0:nx*ny*nz*nt-1))
      do l=1,nt
      do k=1,nz
      do j=1,ny
      do i=1,nx
          x = position(i,j,k,l)
          do d=2,8,2
              call generate_group_element(U(d,x)%a)
              call hermitian_conjugate(U(d,x)%a,U(d-1,x+incrementTable(x,d))%a)
          end do
      end do
      end do
      end do
      end do
      
      end subroutine initiate_lattice_hot

!=====LOAD THE LATTICE FROM A FILE
      subroutine load_lattice(filename)
      character(len=1024),intent(in) :: filename
      logical :: file_exist

      inquire(file=filename,exist=file_exist)
      if (file_exist) then
         allocate(U(8,0:nx*ny*nz*nt-1))
         open(unit=1,file=filename,form='unformatted',status='old')
         read(1) U
         close(1)
      else
         write(6,*) "File ",filename,"not found. Aborting."
         call exit(-1)
      end if

      end subroutine

!=====INITIATE AUXILIARY TABLES
      subroutine initiate_auxTables()
      implicit none
      integer :: i,j,k,l,x

      !Table position(nx,ny,nz,nt): Maps the position x = nx,ny,nz,nt
      !into an integer site index. This index is what we use across
      !all further computations
      
      allocate(position(nx,ny,nz,nt))
      do l=1,nt
      do k=1,nz
      do j=1,ny
      do i=1,nx
        position(i,j,k,l) = (i-1) + nx*(j-1)+nx*ny*(k-1) + nx*ny*nz*(l-1)
      end do
      end do
      end do
      end do
      
      !incrementTable(x,dir) gives the site ahead of position x in the 
      !direction dir/2, if dir is even. If dir is odd, then gives the site
      !behind x in the direction (dir+1)/2

      allocate(incrementTable(0:nx*ny*nz*nt-1,8))
      
      !Backaward at x direction
      do l=1,nt
      do k=1,nz
      do j=1,ny
          x = position(1,j,k,l)
          incrementTable(x,1) = nx-1
          do i=2,nx
              x = position(i,j,k,l)
              incrementTable(x,1) = -1
          end do
      end do
      end do
      end do
      
      !Forward at x direction
      do l=1,nt
      do k=1,nz
      do j=1,ny
          x = position(nx,j,k,l)
          incrementTable(x,2) = 1-nx
          do i=1,nx-1
              x = position(i,j,k,l)
              incrementTable(x,2) = 1
          end do
      end do
      end do
      end do
      
      !Backaward at y direction
      do l=1,nt
      do k=1,nz
      do i=1,nx
          x = position(i,1,k,l)
          incrementTable(x,3) = nx*(ny-1)
          do j=2,ny
              x = position(i,j,k,l)
              incrementTable(x,3) = -nx
          end do
      end do
      end do
      end do
      
      !Forward at y direction
      do l=1,nt
      do k=1,nz
      do i=1,nx
          x = position(i,ny,k,l)
          incrementTable(x,4) = nx*(1-ny)
          do j=1,ny-1
              x = position(i,j,k,l)
              incrementTable(x,4) = nx
          end do
      end do
      end do
      end do
      
      !Backaward at z direction
      do l=1,nt
      do j=1,ny
      do i=1,nx
          x = position(i,j,1,l)
          incrementTable(x,5) = nx*ny*(nz-1)
          do k=2,nz
              x = position(i,j,k,l)
              incrementTable(x,5) = -nx*ny
          end do
      end do
      end do
      end do
      
      !Forward at z direction
      do l=1,nt
      do j=1,ny
      do i=1,nx
          x = position(i,j,nz,l)
          incrementTable(x,6) = nx*ny*(1-nz)
          do k=1,nz-1
              x = position(i,j,k,l)
              incrementTable(x,6) = nx*ny
          end do
      end do
      end do
      end do
      
      !Backaward at t direction
      do k=1,nz
      do j=1,ny
      do i=1,nx
          x = position(i,j,k,1)
          incrementTable(x,7) = nx*ny*nz*(nt-1)
          do l=2,nt
              x = position(i,j,k,l)
              incrementTable(x,7) = -nx*ny*nz
          end do
      end do
      end do
      end do
      
      !Forward at t direction
      do k=1,nz
      do j=1,ny
      do i=1,nx
          x = position(i,j,k,nt)
          incrementTable(x,8) = nx*ny*nz*(1-nt)
          do l=1,nt-1
              x = position(i,j,k,l)
              incrementTable(x,8) = nx*ny*nz
          end do
      end do
      end do
      end do

      end subroutine initiate_auxTables

      end module lattice
