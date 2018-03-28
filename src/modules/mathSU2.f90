!=====THE MATH MODULE=======
!This module contains math functions used for performing operations on
!SU(2) and SU(3) matrices, such as matrices multiplications, linear
!combinations and taking traces

!Needs the Ziggurat library, which can be downloaded from
!http://jblevins.org/mirror/amiller/
!Alan Miller codes are under public domain, so no worries about copyright here



      MODULE mathSU2
      use ziggurat
      implicit none
      real*8 :: Delta = .5
      real*8 :: pi = 3.141592653589793238462643383279d0 !Retrieved from wikipedia. This ought be enough, right?

      CONTAINS

!=====MULTIPLY TWO COMPLEX MATRICES (U3 = U1*U2)
      subroutine matrix_multiply(U1,U2,U3)
      implicit none
      real*8,dimension(4) :: U1,U2,U3

      !c_4 = a_4*b_4-a_i*b_i
      U3(4) = U1(4)*U2(4)-U1(3)*U2(3)-U1(2)*U2(2)-U1(1)*U2(1)

      !c_k  = a_4   * b_k  +  a_k   * b_4   - a_i*b_j*eps_{i j k}
      U3(1) = U1(4) * U2(1) + U1(1) * U2(4) - U1(2) * U2(3) + U1(3) * U2(2)
      U3(2) = U1(4) * U2(2) + U1(2) * U2(4) - U1(3) * U2(1) + U1(1) * U2(3)
      U3(3) = U1(4) * U2(3) + U1(3) * U2(4) - U1(1) * U2(2) + U1(2) * U2(1)
      end subroutine matrix_multiply

!====TAKE THE TRACE OF A MATRIX

      real*8 function Tr(U)
      implicit none
      real*8,dimension(4) :: U

      Tr = 2.d0*U(4)

      end function Tr

!====GENERATES A NORMALIZED RANDOM VECTOR
      subroutine generate_group_element(U)
      implicit none
      real*8,dimension(4) :: U
      !real*8,dimension(3:4) :: r
      real*8 :: Norm !For use with gaussiand distribution method
      integer :: i
      !real*8 :: NormA,NormB For use with Marsaglia method

!     Marsaglia method (marginally inferior - 3% slower)
!      i = 0
!      do while (i .ne. 1)
!        U(1) = 2.d0*uni( )-1.d0
!        U(2) = 2.d0*uni( )-1.d0
!        r(3) = 2.d0*uni( )-1.d0
!        r(4) = 2.d0*uni( )-1.d0
!        NormA = U(1)**2 + U(2)**2
!        NormB = r(3)**2 + r(4)**2
!
!        if ( (NormA .lt. 1.0) .and. (NormB .lt. 1.0) ) then
!            U(3) = r(3)*dsqrt((1-NormA)/(NormB))
!            U(4) = r(4)*dsqrt((1-NormA)/(NormB))
!            i = 1
!        end if
!    end do

!Using random normal distribution method
      U(1) = rnor( )
      U(2) = rnor( )
      U(3) = rnor( )
      U(4) = rnor( )

!     Normalization of the vector
      Norm = dsqrt(U(1)**2.d0+U(2)**2.d0+U(3)**2.d0+U(4)**2.d0)
      U(1) = U(1)/Norm
      U(2) = U(2)/Norm
      U(3) = U(3)/Norm
      U(4) = U(4)/Norm

      end subroutine generate_group_element
      
!====COMMUTATOR
      subroutine commutate(U1,U2,V_out) !Given two matrices parametrized as SU2 group members, commutes these matrices
      implicit none
      real*8,dimension(4),intent(in) :: U1,U2
      real*8,dimension(4),intent(out) :: V_out
      
      V_out(1) = (U1(2)*U2(3)-U1(3)*U2(2))
      V_out(2) = (U1(3)*U2(1)-U1(1)*U2(3))
      V_out(3) = (U1(1)*U2(2)-U1(2)*U2(1))
      V_out(4) = 0.d0
      
      end subroutine commutate

!====GENERATES THE HERMITIAN CONJUGATE OF A MATRIX
      subroutine hermitian_conjugate(U_input,U_output)
      implicit none
      real*8,dimension(4) :: U_input,U_output

      U_output(1) = -U_input(1)
      U_output(2) = -U_input(2)
      U_output(3) = -U_input(3)
      U_output(4) = U_input(4)

      end subroutine hermitian_conjugate

!====SUM 2 SU(2) matrices
      subroutine sumSU2(U1,U2,V_out) ! Notice V_out do not belong to SU(2)
                                     ! a_\mu a_\mu != 1. But it have the same structure V_out = a_i * sigma_i + a_4 * identity

      implicit none
      real*8,dimension(4),intent(in) :: U1,U2
      real*8,dimension(4),intent(out) :: V_out

      V_out(1) = U1(1)+U2(1)
      V_out(2) = U1(2)+U2(2)
      V_out(3) = U1(3)+U2(3)
      V_out(4) = U1(4)+U2(4)


      end subroutine sumSU2

!====SUBTRACT 2 SU(2) matrices
      subroutine subtractSU2(U1,U2,V_out) ! Notice V_out do not belong to SU(2)
                                     ! a_\mu a_\mu != 1. But it have the same structure V_out = a_i * sigma_i + a_4 * identity

      implicit none
      real*8,dimension(4),intent(in) :: U1,U2
      real*8,dimension(4),intent(out) :: V_out

      V_out(1) = U1(1)-U2(1)
      V_out(2) = U1(2)-U2(2)
      V_out(3) = U1(3)-U2(3)
      V_out(4) = U1(4)-U2(4)


      end subroutine subtractSU2      
      
!=====COMPUTES THE DETERMINANT OF A MATRIX THAT CAN BE PARAMETRIZED IN A SIMILAR WAY AS ONE BELONGING TO SU(2)
      real*8 function detSU2likeMatrix(Uin)
      implicit none
      real*8,dimension(4),intent(in) :: Uin

      detSU2likeMatrix = Uin(1)**2+Uin(2)**2+Uin(3)**2+Uin(4)**2

      end function detSU2likeMatrix

!=====RANDOMLY CHANGE THE SPATIAL COMPONENT OF THE SU(2) PARAMETRIZATION VECTOR, KEEPING THE "TEMPORAL" ONE UNTOUCHED
      subroutine randPointSphereMarsagliaMethod(U)
      real*8,dimension(4),intent(inout) :: U
      real*8,dimension(3) :: temp
      real*8,dimension(2) :: r
      real*8 :: radius
      logical :: accepted

      accepted = .false.
      radius = dsqrt(1.d0-U(4)**2)
      do while (.not. accepted)
        r(1) = 2.d0*uni( )-1.d0
        r(2) = 2.d0*uni( )-1.d0
        if ( r(1)**2 + r(2)**2 .lt. 1) then
            temp(1) = 2.d0*r(1)*dsqrt(1-r(1)**2-r(2)**2)
            temp(2) = 2.d0*r(2)*dsqrt(1-r(1)**2-r(2)**2)
            temp(3) = 1.d0 - 2.d0*(r(1)**2+r(2)**2)
            accepted = .true.
        end if
      end do

      U(1) = temp(1)*radius
      U(2) = temp(2)*radius
      U(3) = temp(3)*radius

      end subroutine
      
      
!=====GENERATE GROUP ELEMNTS NEAR UNITY
      subroutine generate_group_element_near1(U)
      implicit none
      real*8, dimension(4), intent(inout) :: U
      real*8, dimension(4) :: r
      real*8 :: rAbs
      integer :: i
      
      do i=1,4
          call RANDOM_NUMBER(r(i))
          r(i) = r(i) - 5.d-1
      end do
      rAbs = dsqrt(r(1)**2+r(2)**2+r(3)**2)
      
      do i=1,3
          U(i) = Delta*r(i)/rAbs
      end do
      U(4) = r(4)*dsqrt(1-Delta**2)/dabs(r(4))   
      
      end subroutine

      END MODULE

