!Willian Matioli Serenone

!This program reads data generated from the
!''tmunu_corr.f90'' program

include "mkl_dfti.f90"

program fftprop

use MKL_DFTI !This article may be usefull at
!understanding how to use these libraries
!https://software.intel.com/en-us/articles/the-intel-math-kernel-library-and-its-fast-fourier-transform-routines

real*8,parameter :: pi = 4.d0*datan(1.d0)
integer :: nx, ny, nz, nt
integer :: rawDataSize, stat
complex*16 :: recordSize
real*8,dimension(:),allocatable :: rawData
complex*16,dimension(:),allocatable :: spaceData
integer :: i,j,x,y,z,t,reclen,xS,yS,zS,tS,sourcePos,record_len
character(1024) :: dataFile

integer, dimension(4) :: FFTlen
type(DFTI_DESCRIPTOR), pointer :: descHandler

call readArgs

rawDataSize = nx*ny*nz*nt !Determines the size of the data to be retrieved on each file
allocate(rawData(0:rawDataSize-1)) !Allocates the rawData
allocate(spaceData(0:rawDataSize-1)) !Allocates the data after shifting the source to the origin
!allocate(transformedData(0:rawDataSize-1))!Vector that will store the output of the FFT 

!Used by MKL_DFTI to determine the length of the DFT to be performed
!FFTlen = (/Ns,Ns,Ns,Nt/)

!Links the fortran pointer and the fortran array

!To perform the DFT, we will need to inform the length and domain of the input
!This is done by creating a DESCRIPTOR
stat = DftiCreateDescriptor( descHandler, DFTI_DOUBLE, DFTI_REAL, 4, (/nx,ny,nz,nt/) )
stat = DftiSetValue( descHandler, DFTI_FORWARD_SCALE, 1)
stat = DftiSetValue( descHandler, DFTI_BACKWARD_SCALE, 1)
stat = DftiSetValue( descHandler, DFTI_PLACEMENT, DFTI_INPLACE)
stat = DftiCommitDescriptor( descHandler )

!The index sequence, in the order of the faster running number to the slowest one is: x, y, z, t
!Compute source position
if (sourcePos .gt. nx*ny*nz*nt) then
   print *, "WARNING! SOURCE POSITION OUTSIDE LATTICE. EXITING NOW"
   call EXIT(-1)
end if
tS = sourcePos/(nx*ny*nz)
zS = (sourcePos - tS*nx*ny*nz)/(nx*ny)
yS = (sourcePos - tS*nx*ny*nz - zS*nx*ny)/nx
xS = sourcePos - tS*nx*ny*nz - zS*nx*ny - yS*nx

!Load file
print *, "Loading file..."
inquire(iolength=record_len) pi
open(unit=1,file=trim(dataFile),status='old',form='unformatted',access='direct',recl=record_len)
do j=1,rawDataSize
   read(1,rec=j) rawData(j-1)
end do
close(1)
print *, "File loaded. Shifting source position to origin"

do j=1,rawDataSize
   t = (j-1)/(nx*ny*nz)
   z = (j-1 - t*nx*ny*nz)/(nx*ny)
   y = (j-1 - t*nx*ny*nz - z*nx*ny)/nx
   x = j-1 - t*nx*ny*nz - z*nx*ny - y*nx

!   print *, t,z,y,x
!   print *, ""
   !We will shift the indexes, so the source is on the origin
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

   spaceData(x+y*nx+z*nx*ny+t*nx*ny*nz) = dcmplx(rawData(j-1),0.d0) !MKL_DFT does not like the 4D vector
 !  print *, x+y*nx+z*nx*ny+t*nx*ny*nz
end do

print*, "Done. Computing FFT."
!We may finally perform the fourier transform
stat = DftiComputeForward( descHandler, spaceData )

print *, "Done. Saving file."
!Now we save in the output file
!Notice: Index i has momentum k = 2*pi*i/N, i=0,N-1. Since the input is real data, we keep only the range [0,N/2]. The remaining components are complex conjugates of these.


inquire(iolength=reclen) recordSize
!Now that we finished the computation, we write things to disk, deallocate memory and exit
open(unit=10,file=trim(dataFile)//"inverted.fft",form='unformatted',access='direct',recl=reclen)
do t=0,nt/2
   do z=0,nz/2
      do y=0,ny/2
         do x=0,nx/2
            j=x+y*(nx/2+1)+z*(nx/2+1)*(ny/2+1)+t*(nx/2+1)*(ny/2+1)*(nz/2+1)
            write(10,rec=j+1) spaceData(j)
!            print *, j,spaceData(j)
         end do
      end do
   end do
end do
close(10)
print*, trim(dataFile)
print*, "Done. Have a nice day :)"
!read(5,*)
stat = DftiFreeDescriptor( descHandler )
deallocate(rawData,spaceData)

contains

!=====Subroutine to read input arguments
      SUBROUTINE readArgs()

      implicit none

      !Output variables
      integer, parameter :: minNumberParameters = 6 !Change this if input changes
      character(len=50) :: argNx,argNy,argNz,argNt,argSourcePos

      if (COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
        print*, "It is mandatory to pass at least 7 parameters as input in the format:"
        print*, "nx ny nz nt sourcePos results/Data/FilePath.dat"
        print*, "Exiting now"
        call EXIT(1)
      else
        call GET_COMMAND_ARGUMENT(1,argNx)
        call GET_COMMAND_ARGUMENT(2,argNy)
        call GET_COMMAND_ARGUMENT(3,argNz)
        call GET_COMMAND_ARGUMENT(4,argNt)
        call GET_COMMAND_ARGUMENT(5,argSourcePos)
        call GET_COMMAND_ARGUMENT(6,dataFile)
        read(argNx,*) nx
        read(argNy,*) ny
        read(argNz,*) nz
        read(argNt,*) nt
        read(argSourcePos,*) sourcePos 
      end if
      END SUBROUTINE
end program
