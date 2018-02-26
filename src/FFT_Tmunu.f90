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
integer :: i,j,x,y,z,t,reclen,xS,yS,zS,tS,sourcePos
character(1024) :: dataFile

integer, dimension(4) :: FFTlen
type(DFTI_DESCRIPTOR), pointer :: descHandler

call readArgs

rawDataSize = nx*ny*nz*nt !Determines the size of the data to be retrieved on each file
allocate(rawData(rawDataSize)) !Allocates the rawData
allocate(spaceData(nx*ny*nz*nt)) !Allocates the data after shifting the source to the origin
allocate(transformedData(nx*ny*nz*nt))!Vector that will store the output of the FFT 

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
open(unit=1,file=trim(dataFile),status='old',form='unformatted')
read(1) rawData
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

   spaceData(1+x+y*nx+z*nx*ny+t*nx*ny*nz) = rawData(j) !MKL_DFT does not like the 4D vector
end do

print*, "Done. Computing FFT."
!We may finally perform the fourier transform
stat = DftiComputeForward( descHandler, spaceData, transformedData )

print *, "Done. Saving file."
!Now we save in the output file
!Notice: Index i has momentum k = 2*pi*i/N, i=0,N-1. Since the input is real data, we keep only the range [0,N/2]. The remaining components are complex conjugates of these.


inquire(iolength=reclen) recordSize
!Now that we finished the computation, we write things to disk, deallocate memory and exit
open(unit=1,file=trim(dataFile)//"inverted.fft",form='unformatted',access='direct',recl=reclen)
do t=0,nt/2
   do z=0,nz/2
      do y=0,ny/2
         do x=0,nx/2
            j=1+x+y*nx/2+z*nx*ny/4+t*nx*ny*nz/8
            write(1,rec=j) transformedData(j)
         end do
      end do
   end do
end do
close(1)
print*, "Done. Have a nice day :)"

stat = DftiFreeDescriptor( descHandler )
deallocate(rawData,spaceData,transformedData)

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
