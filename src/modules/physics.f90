!=====THE PHYSICS MODULE======
!This module has the subroutines responsible for computing and updating
!the system action

!-----------------------------------------------------
!=====VARIABLES AND TYPES=====
!-----------------------------------------------------


!-----------------------------------------------------
!=====SUBROUTINES AND FUNCTIONS======
!-----------------------------------------------------
!
!     -plaquette(dir1,dir2,i,j,k,l): Computes the plaquette in site (i,j,k,l)
!and on the plane defined by the directions dir1 and dir2. Returns the value of the plaquette
!P = Tr(U1*U2*U3*U4)


      MODULE physics

      use lattice
      use mathSU2
      use ziggurat

      implicit none

      CONTAINS

!=====ACTION FUNCTION
      real*8 function computeS()
      implicit none
      real*8 :: soma
      real*8, dimension(4) :: plaq
      integer :: i,j,k,l,d1,d2

      soma = 0.d0

      do l=1,nt
      do k=1,nz
      do j=1,ny
      do i=1,nx
          do d1=2,8,2
              do d2=d1+2,8,2
                  call plaquette(d1,d2,i,j,k,l,plaq)
                  soma = soma + Tr(plaq)
              end do
          end do
      end do
      end do
      end do
      end do
      computeS = beta*(dble(nx*ny*nz*nt*6.d0)-soma*.5d0)!/dble(nx*ny*nz*nt*6.d0)

      end function computeS      
    
!=====RETURNS PLAQUETTE U_{mu nu}(m) ON plaq
      subroutine plaquette(dir1,dir2,i,j,k,l,plaq)
      implicit none
      integer, intent(in) :: dir1,dir2,i,j,k,l
      real*8, dimension(4),intent(out) :: plaq
      integer :: x0,x1,x2,x3
      real*8, dimension(4) :: U_temp1,U_temp2
!      integer :: a1,a2



      x0 = position(i,j,k,l)
      x1 = x0 + incrementTable(x0,dir1)
      x2 = x1 + incrementTable(x1,dir2)
      x3 = x0 + incrementTable(x0,dir2)
      

!
!     dir1
!      ^   x1            x2
!      |    ------->------
!      |    |            |
!      |    |            |
!      |    |            |
!      |    ^            | 
!      |    |            |
!      |    |            |
!      |    |            |
!      |    -------<------
!      |   x0            x3
!      |
!      ----------------------------> dir2

      call matrix_multiply( U(dir1,x0)%a, U(dir2,x1)%a , U_temp1 )
!      a1 = dir1+2*mod(dir1,2)-1
!      a2 = dir2+2*mod(dir2,2)-1
      call matrix_multiply( U_temp1, U( dir1+2*mod(dir1,2)-1, x2 )%a ,U_temp2 )
      call matrix_multiply( U_temp2, U( dir2+2*mod(dir2,2)-1, x3 )%a , plaq )
      
      
      
      end subroutine plaquette

!=====COMPUTES ALL STAPLES AROUND THE LINK i,j,k,l,dir
      subroutine computeStaplesSet(i,j,k,l,dir,staples)
      implicit none
      integer, intent(in) :: i,j,k,l,dir
      real*8,dimension(4,6),intent(out) :: staples
      integer :: x0,x1,x2,y0,y1,y2,dir2,o
      real*8, dimension(4) :: U_temp1,U_temp2


      o=1
      x0 = position(i,j,k,l)
      y0 = x0+incrementTable(x0,dir)
      do dir2=2,8,2
          if (dir2 < dir) then


!     dir2
!      ^    x1           x2
!      |    ------->------
!      |    |            |
!      |    |            |
!      |    |            |
!      |    ^            |       ====>   P_{dir2 dir}(n) without link in direction U_{-dir)(m+dir)
!      |    |            |                stored in staples(:, odd)
!      |    |            |                plaquette completed by multiplying staple from left by U_{-dir)(m+dir)
!      |    |            |
!      |    *
!      |    x0
!      |
!      ----------------------------> dir
!                        y0
!                        *
!           |            |
!           |            |
!           |            |
!           ^            |    ====>   P_{-dir2 -dir}(n+dir) without link in direction U_{dir)(m)
!           |            |             stored in staples(:, even)
!           |            |             plaquette completed by multiplying staple from left by U_{dir)(m)
!           |            |              
!           -------<------ 
!          y2            y1
              
              
              !P_{dir2 dir}(n) without link in direction U_{-dir)(m+dir)
              
              x1 = x0 + incrementTable(x0,dir2)
              x2 = x1 + incrementTable(x1,dir)
              call matrix_multiply( U(dir2,x0)%a, U(dir,x1)%a , U_temp1 )
              call matrix_multiply( U_temp1, U( dir2-1,x2)%a ,staples(:,o) )
              
              o=o+1
              !P_{-dir2 -dir}(n+dir) without link in direction U_{dir)(m)
              
              y1 = y0 + incrementTable(y0,dir2-1)
              y2 = y1 + incrementTable(y1,dir-1)
              call matrix_multiply( U(dir2-1,y0)%a, U(dir-1,y1)%a , U_temp1 )
              call matrix_multiply( U_temp1, U( dir2,y2)%a ,staples(:,o) )
              
              o=o+1
          else if (dir2 > dir) then

              
!     dir2              
!      ^    y2           y1
!      |    -------<------
!      |    |            |
!      |    |            |
!      |    |            |
!      |    |            ^       ====>   P_{dir2 -dir}(m+dir) without link in direction U_{dir)(m)
!      |    |            |                stored in staples(:, even)
!      |    |            |                plaquette completed by multiplying staple from left by U_{dir)(m)
!      |    |            |
!      |                 *
!      |    m            y0
!      |                 
!      ----------------------------> dir
!           x0
!           *             
!           |            |
!           |            |
!           |            |
!           |            ^ U3 ====>   P_{-dir2 dir}(m) without link in direction U_{-dir)(m+dir)
!           |            |             stored in staples(:, odd)
!           |            |             plaquette completed by multiplying staple from left U_{-dir)(m+dir)
!           |            |              
!           ------->------ 
!          x1            x2
              
              
              !P_{-dir2 dir}(m) without link in direction U_{-dir)(m+dir)
              
              x1 = x0 + incrementTable(x0,dir2-1)
              x2 = x1 + incrementTable(x1,dir)
              call matrix_multiply( U(dir2-1,x0)%a, U(dir,x1)%a , U_temp1 )
              call matrix_multiply( U_temp1, U( dir2,x2)%a ,staples(:,o) )
              
              o=o+1
              !P_{-dir2 -dir}(n+dir) without link in direction U_{dir)(m)
              
              y1 = y0 + incrementTable(y0,dir2)
              y2 = y1 + incrementTable(y1,dir-1)
              call matrix_multiply( U(dir2,y0)%a, U(dir-1,y1)%a , U_temp1 )
              call matrix_multiply( U_temp1, U(dir2-1,y2)%a ,staples(:,o) )
              
              o=o +1         
          
          end if
      end do

      end subroutine computeStaplesSet

!=====COMPUTE MEAN FIELD
      subroutine computeMeanField(staples,mField)
      implicit none
      real*8,dimension(4,6),intent(in) :: staples
      real*8, dimension(4), intent(out) :: mField
      real*8, dimension(4) :: Uaux
      integer :: m

      !computes staples
      do m=1,4
        mField(m) = 0.d0
      end do
      do m=1,5,2
        call hermitian_conjugate(staples(:,m),Uaux)  !In order to factorate the updated link, one must take the staple dagger
                                                     !Notice this does not affect the result of the trace 
        call sumSU2(mField, Uaux, mField)
      end do
      
      do m=2,6,2
        call sumSU2(mField, staples(:,m), mField)
      end do
      end subroutine computeMeanField

!=====COMPUTE NEW LINK VARIABLE FROM MEAN FIELD --- GATTRINGER BOOK
      subroutine computeNewU(c,meanField,UPrime)
      real*8,intent(in) :: c
      real*8,dimension(4),intent(in) :: meanField
      real*8,dimension(4),intent(out) :: UPrime
      real*8,dimension(4) :: Uaux,Uaux2,Ubar
      real*8 :: lambdaSquare,aux,temp
      real*8,dimension(4) :: r
      integer :: i

      do i=1,4
          call RANDOM_NUMBER(r(i))
          r(i) = 1-r(i) !r(i) is in [0,1[ range. This put it on the desired ]0,1] range
      end do
      
      lambdaSquare = - (dlog(r(1)) + ((dcos(2*pi*r(2)))**2)*dlog(r(3)))/(2.d0*c*beta)
      if (r(4)**2 .le. 1-lambdaSquare ) then
          Uaux(4) = 1.d0-2.d0*lambdaSquare
          call randPointSphereMarsagliaMethod(Uaux)
          !compute Ubar = meanField/c
          Ubar = meanField/c
          call hermitian_conjugate(Ubar,Uaux2)
          !Ubar = Uaux2
          !New lattice site
          call matrix_multiply(Uaux,Uaux2,UPrime)
      end if  

      end subroutine computeNewU


!=====UPDATE S GIVEN A NEW LINK AND A MEAN FIELD (A SUM OF STAPLES SURROUNDING THE UPDATED LINK)
      subroutine updateSMeanField(i,j,k,l,dir,S,staples,UPrime)
      real*8,dimension(4,6),intent(in) :: staples
      integer,intent(in) :: i,j,k,l,dir
      real*8,intent(inout) :: S
      real*8, dimension(4),intent(in) :: UPrime
      real*8, dimension(4) :: Uaux
      integer :: o,x,y
    
      x = position(i,j,k,l)
      y = x+incrementTable(x,dir)

      !Subtracts plaquettes contribution to the action that will be changed
      do o=1,5,2
          call  matrix_multiply(staples(:,o),U(dir-1,y)%a,Uaux)
          S = S - beta*(1-Tr(Uaux)*5.d-1)
      end do
      
      do o=2,6,2
          call  matrix_multiply(staples(:,o),U(dir,x)%a,Uaux)
          S = S - beta*(1-Tr(Uaux)*5.d-1)
      end do
      

!     Updates the link with the new UPrime (and its dagger, ie the link running in the backward direction of the point in front of it)
      U(dir,x)%a = UPrime
      call hermitian_conjugate(UPrime, U(dir-1,y)%a)
!      S = computeS()


!     Add again the plaquettes that were previously removed
      do o=1,5,2
          call  matrix_multiply(staples(:,o),U(dir-1,y)%a,Uaux)
          S = S + beta*(1-Tr(Uaux)*5.d-1)
      end do
      
      do o=2,6,2
          call  matrix_multiply(staples(:,o),U(dir,x)%a,Uaux)
          S = S + beta*(1-Tr(Uaux)*5.d-1)
      end do
      
      
      !if ( dabs(computeS() - S) > 1.d-3 ) then
      !    print *, "Meaningfull difference detected on Action calculation detected."
      !    print *, S-ComputeS()
      !    read(5,*)
      !end if

      end subroutine updateSMeanField

!=====COMPUTE NEW LINK VARIABLE FROM MEAN FIELD --- CREUTZ PAPER
      subroutine computeNewU_Creutz(c,meanField,UPrime)
      real*8,intent(in) :: c
      real*8,dimension(4),intent(in) :: meanField
      real*8,dimension(4),intent(out) :: UPrime
      real*8,dimension(4) :: Uaux,Uaux2,Ubar
      real*8 :: r,r2,aux,temp

      !Generate random uniform number in the interval [exp(-beta c),1]
      
      aux = dexp(-2.d0*beta*c)
100   call random_number(r)
      r = r*(1.d0-aux) + aux
      
      !Compute a0 using r
      Uaux(4) = 1.d0+dlog(r)/(beta*c)
      call random_number(r2)
     
      !"Accept with probability sqrt(1-Uaux(4)**2)"
      if (r2 .lt. dsqrt(1-Uaux(4)**2) ) then
        !"Accept it if r < sqrt(1-b^-2 c^-2 ln^2 r). Else, draw another number and repeat.
        call randPointSphereMarsagliaMethod(Uaux)
      else
         go to 100 !r fails our condition, hence we must draw another number
      end if
      
      !compute Ubar = meanField/c
      Ubar = meanField/c
      call hermitian_conjugate(Ubar,Uaux2)
      Ubar = Uaux2
      !New lattice site
      call matrix_multiply(Uaux,Ubar,UPrime)

      end subroutine computeNewU_Creutz
      
    
      END MODULE
