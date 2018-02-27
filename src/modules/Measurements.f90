module Measurements
    use physics
    use lattice
    use mathSU2
    implicit none
    
    real*8, dimension(:,:), allocatable :: V


    contains
    
 
!===COMPUTE Fmunu(m)
    subroutine ComputeFmunu(mu,nu,i,j,k,l,F)
    integer, intent(in) :: mu,nu,i,j,k,l
    type(su2matrix), intent(out) :: F
    integer :: x,m
    real*8, dimension(4) :: plaq,Q1,Q2
    
    x=position(i,j,k,l)
    do m=1,4
        Q1(m) = 0.d0
        Q2(m) = 0.d0
    end do
    
    !Starting computing first clover
    call plaquette(2*mu,2*nu,i,j,k,l,plaq) !First leaf
    call sumSU2( Q1, plaq, Q1 )
    
    call plaquette(2*nu,2*mu-1,i,j,k,l,plaq)! Second leaf
    call sumSU2( Q1, plaq, Q1 )
    
    call plaquette(2*mu-1,2*nu-1,i,j,k,l,plaq)! Third leaf
    call sumSU2( Q1, plaq, Q1 )
    
    call plaquette(2*nu-1,2*mu,i,j,k,l,plaq)! Fourth leaf
    call sumSU2( Q1, plaq, Q1 )
    
    
    !Starting computing second clover
    call plaquette(2*nu,2*mu,i,j,k,l,plaq) !First leaf
    call sumSU2( Q2, plaq, Q2 )
    
    call plaquette(2*mu,2*nu-1,i,j,k,l,plaq)! Second leaf
    call sumSU2( Q2, plaq, Q2 )
    
    call plaquette(2*nu-1,2*mu-1,i,j,k,l,plaq)! Third leaf
    call sumSU2( Q2, plaq, Q2 )
    
    call plaquette(2*mu-1,2*nu,i,j,k,l,plaq)! Fourth leaf
    call sumSU2( Q2, plaq, Q2 )
 
    
    !We invert the signal of Q2
    do m=1,4
        Q2(m) = -Q2(m)
    end do
    
    !And sum them
    call sumSU2(Q1,Q2,F%a)
    
    !Finally, divide everything by 8
    do m=1,4
        F%a(m) = -F%a(m)/8.d0
    end do
    end subroutine
    
!===Compute Off-Diagonal tensor components at temporal direction of the entire lattice
    subroutine CalcT0i(T0i)
    integer :: mu,nu,x,i,j,k,l
    real*8, dimension(3,0:nx*ny*nz*nt-1), intent(out) :: T0i
    type(su2matrix),dimension(3,0:nx*ny*nz*nt-1) :: F0i
    real*8, dimension(4) ::Uaux
    
    !Compute Fmunu over the entire lattice
    do l=1,nt
    do k=1,nz
    do j=1,ny
    do i=1,nx
        do mu=1,3
            x = position(i,j,k,l)
            call ComputeFmunu(4,mu,i,j,k,l,F0i(mu,x))
        end do
    end do
    end do
    end do
    end do
        
            
    !Compute T0i over the entire lattice
    do x=0,nx*ny*nz*nt-1
        do mu=1,3
            call matrix_multiply(F0i(mu,x)%a,F0i(mu,x)%a,Uaux)
            T0i(mu,x) = beta*Tr(Uaux)/2.d0
        end do
    end do

    end subroutine
    
    
!===Compute a Line starting at "x0", going in direction "dir" and of size "size"
    subroutine Line(dir,x,size,WL)
    implicit none
    integer, intent(in) :: dir,x,size
    type(su2matrix),intent(out) :: WL
    type(su2matrix) :: Uaux
    integer :: x0,x1,i
    
    x0 = x
    WL=U(dir,x0)
     
    do i=1,size-1
        x1 = x0+incrementTable(x0,dir)
        call matrix_multiply(WL%a,U(dir,x1)%a,Uaux%a)
        WL = Uaux
        x0 = x1
    end do
       
    end subroutine

!===Computes polyakov loop 
    real*8 function polyakovLoop()
    implicit none
    type(su2matrix) :: TL
    type(su2matrix) :: Uaux
    integer :: x0,x1,i,j,k,l
    polyakovLoop = 0.d0
    
    do k=1,nz
        do j=1,ny
            do i=1,nx
                x0 = position(i,j,k,1)
                TL=U(8,x0)
                do l=1,nt-1
                    x1 = x0+incrementTable(x0,8)
                    call matrix_multiply(TL%a,U(8,x1)%a,Uaux%a)
                    TL = Uaux
                    x0 = x1
                end do
                polyakovLoop = polyakovLoop + Tr(TL%a)
            end do
        end do
    end do
    
    polyakovLoop=polyakovLoop/dble(nx*ny*nz)    
    
    end function
    
!===Computes the WilsonLoop
    !Note: returns the Trace of the Wilson Loop of size r by t.
    !We also average the loop over the entire lattice and change the direction over the 3 spatial directions
    !Notice
    real*8 function latticePotential(r,t)
    implicit none
    integer,intent(in) :: r,t
    type(su2matrix) :: WL1,WL2,TT1,TT2 !The 2 Wilson Lines and the 2 Temporal Transporters
    type(su2matrix) :: Uaux1,Uaux2
    integer :: x,i,j,k,l,dir,shifter
    
    latticePotential = 0.d0
    !x=position(nx/2,ny/2,nz/2,nt/2)

    
    do l=1,nt
    do k=1,nz
        do j=1,ny
            do i=1,nx
                dir=6
    !             do dir=2,6,2
                    x=position(i,j,k,l)
                    call Line(dir,x,r,WL1)
                    do shifter=1,r
                        x=x+incrementTable(x,dir)
                    end do
                    call Line(8,x,t,TT1)
                    do shifter=1,t
                        x=x+incrementTable(x,8)
                    end do
                    call Line(dir-1,x,r,WL2)
                    do shifter=1,r
                        x=x+incrementTable(x,dir-1)
                    end do
                    call Line(7,x,t,TT2)
                    call matrix_multiply(WL1%a,TT1%a,Uaux1%a)
                    call matrix_multiply(Uaux1%a,WL2%a,Uaux2%a)
                    call matrix_multiply(Uaux2%a,TT2%a,Uaux1%a)
                    latticePotential = latticePotential + Tr(Uaux1%a)
                !end do
            end do
        end do
    end do
    end do
    
    latticePotential=latticePotential/dble(nx*ny*nz*nt)
    
    end function
end module
