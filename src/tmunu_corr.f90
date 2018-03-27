!This program is devoted to the comp-utation of the correlation function
! T0i(x)T0j(0) in each point for one lattice configuration.
! 0 is the source and we will ask the user to either define which point on
! the lattice correspond to it, or set it randomly. The source position will
! be encoded on the file name

program tmunu_corr

use IOfunctions
use lattice
use Measurements
implicit none
integer,parameter :: compute_all_sources = 1
character(len=1024) :: latticeFile = ''
character(len=50) :: filename = ''
real*8, allocatable, dimension(:,:) :: T0i
real*8, allocatable, dimension(:,:,:) :: T0iT0j,aux
!integer, allocatable, dimension (:,:) :: coordSum !Returns the site obtained by summing sites a+b
integer :: s,seed2,x,y,z,t,d1,d2,record_len,tS,zS,yS,xS,i
integer :: a,b,ab,ax,ay,az,at,bx,by,bz,bt,abx,aby,abz,abt
!Load parameters (lattice size and lattice file name)
call readArgs() 

!Allocates lattice and loads file
call initiate_auxTables()
call load_lattice(latticeFile)

!Computes T0i over the entire lattice
allocate(T0i(3,0:nx*ny*nz*nt-1))
allocate(T0iT0j(3,3,0:nx*ny*nz*nt-1))
<<<<<<< HEAD

select case (compute_all_sources)
   case (0) !What we do if we use the "random source method"
      !Compute T0i
      call CalcT0i(T0i)
      !Compute the correlation function
      do x=0,nx*ny*nz*nt-1
         do d2=1,3
            do d1=1,3
               T0iT0j(d1,d2,x) = T0i(d1,x)*T0i(d2,s)
            end do
         end do
      end do
   
   case (1) !What we do if we use the "average over sources", without using the shift

      !Variables initialization
      T0iT0j = 0.d0
 !     allocate(coordSum(0:nx*ny*nz*nt-1,0:nx*ny*nz*nt-1)) !Do not create a table of neighbour
                                                           !For a reasonable lattice it gets giant (32^4 => ~ 4 TB)
      !Computation of T0i
      call CalcT0i(T0i)


      !Sum of therms
      do at=0,nt-1
         do az=0,nz-1
            do ay=0,nx-1
               do ax=0,nx-1
                  a = ax + ay*nx + az*nx*ny + at*nx*ny*nz
                  do bt=0,nt-1
                     do bz=0,nz-1
                        do by=0,nx-1
                           do bx=0,nx-1
                              abx = ax+bx
                              abx = abx-nx*(abx/nx)
                              aby = ay+by
                              aby = aby-ny*(aby/ny)
                              abz = az+bz
                              abz = abz-nz*(abz/nz)
                              abt = at+bt
                              abt = abt-nt*(abt/nt)
                              b = bx + by*nx + bz*nx*ny + bt*nx*ny*nz
                              ab = abx + aby*nx + abz*nx*ny + abt*nx*ny*nz
                              do d1=1,3
                                 do d2=1,3
                                    T0iT0j(d1,d2,a) = T0iT0j(d1,d2,a) + T0i(d1,ab)*T0i(d2,b)
                                 end do
                              end do
                           end do
                        end do
                     end do
                  end do
               end do
            end do
         end do
      end do
 
      !Normalization, so the sum can be interpreted as an average
      T0iT0j = T0iT0j/dble(nx*ny*nz*nt)
      s = 0 !Source is effectivelly at 0
end select


inquire(iolength=record_len) T0iT0j(d1,d2,1)

=======

if (compute_all_sources) then
   s = 0
   allocate(aux(3,3,0:nx*ny*nz*nt-1))
   aux = 0.d0
end if


call CalcT0i(T0i)
do while (s .lt. nx*ny*nz*nt)
   do x=0,nx*ny*nz*nt-1
      do d2=1,3
         do d1=1,3
            T0iT0j(d1,d2,x) = T0i(d1,x)*T0i(d2,s)
         end do
      end do
   end do

   !If computing all sources, we shift it to the origin and averages the result
   if (compute_all_sources) then
      tS = s/(nx*ny*nz)
      zS = (s - tS*nx*ny*nz)/(nx*ny)
      yS = (s - tS*nx*ny*nz - zS*nx*ny)/nx
      xS = s - tS*nx*ny*nz - zS*nx*ny - yS*nx
      do i=0,nx*ny*nz*nt-1
         t = i/(nx*ny*nz)
         z = (i - t*nx*ny*nz)/(nx*ny)
         y = (i - t*nx*ny*nz - z*nx*ny)/nx
         x = i - t*nx*ny*nz - z*nx*ny - y*nx

         if (tS .gt. t) then
            t = nt + t - tS
         else
            t = t - tS
         end if

         if (zS .gt. z) then
            z = nz + z - zS
         else
            z = z - zS
         end if

         if (yS .gt. y) then
            y = ny + y - yS
         else
            y = y - yS
         end if

         if (xS .gt. x) then
            x = nx + x - xS
         else
            x = x - xS
         end if
         
         x = x + y*nx + z*nx*ny + t*nx*ny*nz
         do d2=1,3
            do d1=1,3
               aux(d1,d2,x) = aux(d1,d2,x) + T0iT0j(d1,d2,i)
            end do
         end do

      end do
      s = s+1
   else
      s = s+nx*ny*nz*nt
   end if
end do

if (compute_all_sources) then
   T0iT0j = aux/dble(nx*ny*nz*nt)
   s = 0
else
   s = s-nx*ny*nz*nt
end if

inquire(iolength=record_len) T0iT0j(d1,d2,1)

>>>>>>> 11032376fa265263fe636415c45034294f274dce
do d1=1,3
   do d2=1,3
      write(filename,"('source',I5.5,'ij',I1.1,I1.1,'.dat')") s,d1,d2
      open(unit=11,file=trim(latticeFile)//trim(filename),form="unformatted",access='direct',recl=record_len)
      do x=0,nx*ny*nz*nt-1
         write(11,rec=x+1) T0iT0j(d1,d2,x)
      end do
      close(11)
   end do
end do

contains
   subroutine readArgs()
      integer, parameter :: minNumberParameters = 6
      character(len=50) :: argNx,argNy,argNz,argNt,argSourcePos,argBeta
      real :: r
      if(COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
         print*, "It is mandatory to pass 6 arguments in the format"
         print*, "nx ny nz nt beta path/to/Lattice/File [Source Position Index]"
         print*, "Exiting now"
         call EXIT(1)
      else
      
         call GET_COMMAND_ARGUMENT(1,argNx)
         call GET_COMMAND_ARGUMENT(2,argNy)
         call GET_COMMAND_ARGUMENT(3,argNz)
         call GET_COMMAND_ARGUMENT(4,argNt)
         call GET_COMMAND_ARGUMENT(5,argBeta)
         call GET_COMMAND_ARGUMENT(6,latticeFile)
         call GET_COMMAND_ARGUMENT(7,argSourcePos)

         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt
         read(argBeta,*) beta
         if (argSourcePos .ne. '') then
            read(argSourcePos,*) s
            if (s .gt. nx*ny*nz*nt) then
               write(6,*) "Supplied source position is greater than lattice size. Halting now."
               call EXIT(1)
            end if
         else
            call init_random_seed(seed2)
            CALL RANDOM_NUMBER(r)
            s = int(real(nx*ny*nz*nt-1)*r)
         end if  

      end if
   end subroutine
end program

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
!            write(6,*) seed2
            DEALLOCATE(seed)
      END SUBROUTINE


