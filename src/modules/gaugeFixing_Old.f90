      !Los alamos method of gauge fixing
      !Algorithm extracted from: http://dx.doi.org/10.1016/0550-3213(96)00177-0

      module gaugeFixing

      use lattice

      implicit none
      type(su2matrix), allocatable, dimension(:,:,:,:) :: g,g_new

      contains

      !Initiates variables needed to perform gauge fixing
      subroutine prepGaugeFixing()
      implicit none
      integer :: i,j,k,l

        allocate (g(nx,ny,nz,nt))
        allocate (g_new(nx,ny,nz,nt))
        do i=1,nx
        do j=1,ny
        do k=1,nz
        do l=1,nt
            g(nx,ny,nz,nt)%a(1)=0.d0
            g(nx,ny,nz,nt)%a(2)=0.d0
            g(nx,ny,nz,nt)%a(3)=0.d0
            g(nx,ny,nz,nt)%a(4)=1.d0
        end do
        end do
        end do
        end do

      end subroutine prepGaugeFixing

      subroutine applyGaugeFixing()
        type(su2matrix) :: h,Uaux1
        integer :: i,j,k,l,d
        double precision :: det

        !We need to compute the gauge transform g(x) for
        !each lattice point

        !Second step is, for each site, compute an effective magnetic field
        do i=1,nx
        do j=1,ny
        do k=1,nz
        do l=1,nt
            do d=1,4
                h%a(d) = 0.d0
            end do

            !The comuptation should be U_mu(x)*g^\dagger(x+a_mu) + U_mu(x-a_\mu)*g^\dagger(x-a_mu)
            !But since we will interact only once, g = g_\dagger = 1 acrross the entire lattice and
            !thus we ommit it

            call hermitian_conjugate(U(1,neighTable%x(i,-1),j,k,l)%a, Uaux1%a)
            call sumSU2(U(1,i,j,k,l)%a, Uaux1%a, h%a)

            call hermitian_conjugate(U(2,i,neighTable%y(j,-1),k,l)%a, Uaux1%a)
            call sumSU2(U(2,i,j,k,l)%a, Uaux1%a, h%a)

            call hermitian_conjugate(U(3,i,j,neighTable%z(k,-1),l)%a, Uaux1%a)
            call sumSU2(U(3,i,j,k,l)%a, Uaux1%a, h%a)

            call hermitian_conjugate(U(4,i,j,k,neighTable%t(l,-1))%a, Uaux1%a)
            call sumSU2(U(4,i,j,k,l)%a, Uaux1%a, h%a)

            !The new gauge transform is the transpose hermitian of it, divided by the determinant

            call hermitian_conjugate(h%a, Uaux1%a)
            det = detSU2likeMatrix(Uaux1%a)
            do d=1,4
                g_new(i,j,k,l)%a(d) = Uaux1%a(d)/det
            end do
        end do
        end do
        end do
        end do
      end subroutine


      end module gaugeFixing
