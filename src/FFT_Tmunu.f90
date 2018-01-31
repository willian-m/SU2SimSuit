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
real*8,dimension(:),allocatable :: spaceData, rawData
complex*16,dimension(:),allocatable :: transformedData
complex*16,dimension(:,:,:,:),allocatable :: FFTData
integer :: i,j,x,y,z,t,reclen,xS,yS,zS,tS,sourcePos
character(1024) :: dataFile

integer, dimension(4) :: FFTlen
type(DFTI_DESCRIPTOR), pointer :: descHandler

call readArgs

rawDataSize = nx*ny*nz*nt !Determines the size of the data to be retrieved on each file
allocate(rawData(rawDataSize)) !Allocates the rawData
allocate(spaceData(nx*ny*nz*nt)) !Allocates the data after shifting the source to the origin
allocate(transformedData(nx*ny*nz*nt))!Vector that will store the output of the FFT 
allocate(FFTData(0:nx/2,0:ny/2,0:nz/2,0:nt/2)) !The domain of the input is real => Relevant output is in [0,n/2]

!Used by MKL_DFTI to determine the length of the DFT to be performed
!FFTlen = (/Ns,Ns,Ns,Nt/)

!Links the fortran pointer and the fortran array

!To perform the DFT, we will need to inform the length and domain of the input
!This is done by creating a DESCRIPTOR
stat = DftiCreateDescriptor( descHandler, DFTI_DOUBLE, DFTI_REAL, 4, (/nx,ny,nz,nt/) )
stat = DftiSetValue( descHandler, DFTI_FORWARD_SCALE, 1)
stat = DftiSetValue( descHandler, DFTI_BACKWARD_SCALE, 1)
stat = DftiSetValue( descHandler, DFTI_PLACEMENT, DFTI_NOT_INPLACE)
stat = DftiCommitDescriptor( descHandler )

!The index sequence, in the order of the faster running number to the slowest one is: x, y, z, t
!Compute source position
tS = sourcePos/(nx*ny*nz)
zS = (sourcePos - tS*nx*ny*nz)/(nx*ny)
yS = (sourcePos - tS*nx*ny*nz - zS*nx*ny)/ny
xS = sourcePos - tS*nx*ny*nz - zS*nx*ny - yS*nx

!Load file
open(unit=1,file=trim(dataFile))
read(1) rawData
close(1)

do j=1,rawDataSize
   t = (j-1)/(nx*ny*nz)
   z = (j-1 - t*nx*ny*nz)/(nx*ny*nz)
   y = (j-1 - t*nx*ny*nz - z*nx*ny)/ny
   x = j-1 - t*nx*ny*nz - z*nx*ny - y*nx

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

   spaceData(1+x+y*nx+z*nx*ny+t*nx*ny*nz) = rawData(j) !MKL_DFT does not like the 4D vector
end do

!We may finally perform the fourier transform
stat = DftiComputeForward( descHandler, spaceData, transformedData )

!Now we save in the FFTData array
!Notice: Index i has momentum k = 2*pi*i/N, i=0,N-1. But we want data in range[-N/4,N/4]. We convert to negative momentum by subtracting N

do t=0,nt/2
   do z=0,nz/2
      do y=0,ny/2
         do x=0,nx/2
            FFTData(x,y,z,t) = spaceData(j)
         end do
      end do
   end do
end do


inquire(iolength=reclen) recordSize
!Now that we finished the computation, we write things to disk, deallocate memory and exit
open(unit=1,file=trim(dataFile)//"inverted.fft",form='unformatted')
write(1) FFTData
close(1)

deallocate(rawData,spaceData,transformedData,FFTData)
stat = DftiFreeDescriptor( descHandler )

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
