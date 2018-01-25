!=====THE MONTE CARLO MODULE======
!This module contains the subroutines to change lattice configurations and
!perform Monte Carlo Steps.

!-----------------------------------------------------
!=====VARIABLES AND TYPES=====
!-----------------------------------------------------
!     -logical methodChooser: True to choose Heat-Bath. False to choose Metropolis
!-----------------------------------------------------
!=====SUBROUTINES AND FUNCTIONS======
!-----------------------------------------------------
!
!     -Metropolis_Hit(i,j,k,l,dir,S,rejection_count): Changes the link in the
!direction "dir" in the site (i,j,k,l) and uses the action "S" to decides if the
!update is kept or rejected, according the Metropolis algorithm. The rejection count
!is kept in the variable "rejection_count".
!     -MetropolisIterator: Executes the Metropolis_Hit

      MODULE MonteCarlo
      use physics
!      use gaugeFixing

      implicit none

      CONTAINS

      !=====MAKE A HEAT-BATH HIT IN EACH SITE OF THE LATTICE
      subroutine heatBathIterator()
      implicit none
      integer :: i,j,k,l,d,x
      double precision :: S,c
      double precision,dimension(4) :: meanField,UPrime
      double precision,dimension(4,6) :: staples

      !Makes the loop hitting the entire lattice
      do i=1,nx
      do j=1,ny
      do k=1,nz
      do l=1,nt
      do d=2,8,2
        !Heat-Bath algorithm
        !http://journals.aps.org/prd/pdf/10.1103/PhysRevD.21.2308
          
        x = position(i,j,k,l) 
        call computeStaplesSet(i, j, k, l, d, staples)
        call computeMeanField(staples,meanField)
        c = dsqrt(detSU2likeMatrix(meanField))
        call computeNewU(c,meanField,U(d,x)%a)
        call hermitian_conjugate(U(d,x)%a,U(d-1,x+incrementTable(x,d))%a)
        !call updateSMeanField(i,j,k,l,d,S,staples,UPrime)
      end do
      end do
      end do
      end do
      end do


      end subroutine

      
      
!=====MAKE A METROPOLIS HIT IN EACH SITE OF THE LATTICE --------> NOT WORKING
      subroutine metropolisIterator(S)
      implicit none
      double precision, intent(inout) :: S
      double precision ::oldS,DeltaS,r
      integer :: i,j,k,l,d,x
      double precision,dimension(4) :: oldU,oldUDagger,Uaux
      double precision,dimension(4,6) :: staples
      
      
      
      do l=1,nt
      do k=1,nz
      do j=1,ny
      do i=1,nx
      do d=2,8,2
          oldS = S
          x = position(i,j,k,l)
          oldU = U(d,x)%a
          oldUDagger = U(d-1,x+incrementTable(x,d))%a
          call computeStaplesSet(i, j, k, l, d, staples)
          call generate_group_element_near1(UAux)
          call matrix_multiply(UAux,oldU,U(d,x)%a)
          call updateSMeanField(i,j,k,l,d,S,staples,U(d,x)%a)
          DeltaS = S- oldS
          call RANDOM_NUMBER(r)
          if (r > dexp(-DeltaS)) then !If true, we reject the proposal
              S = oldS
              U(d,x)%a = oldU
              call hermitian_conjugate(U(d,x)%a,U(d-1,x+incrementTable(x,d))%a)
          end if
      end do
      end do
      end do
      end do
      end do
      
          
      
      end subroutine
      END MODULE
