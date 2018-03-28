!Willian Matioli Serenone
!Instituto de Fisica de Sao Carlos

!This program performs statiscal average of scalars observables.
!For non-scalar observables, you may save each component in a file
!And call this program for each component.


program StatisticalAverager
implicit none
integer :: nx,ny,nz,nt, NMCStart, NMCEnd, step, Nbins !Input parameters
integer :: NMC,x, reclen, m, binIndex, numConf
character(len=1024) :: filepath, filename
real*8 :: reErr,imErr
real*8, allocatable, dimension (:) :: average,rawData,errors
real*8, allocatable, dimension (:,:) :: binnedData

call readParameters
numConf = (NMCEnd-NMCStart)/step


print*, "Allocating arrays..."
allocate( average(nx*ny*nz*nt) )
allocate( rawData(nx*ny*nz*nt) )
allocate( errors(nx*ny*nz*nt) )
allocate( binnedData(nx*ny*nz*nt,Nbins))

average = 0.d0
binnedData = 0.d0 
errors = 0.d0 

print *, "Performing statistical average..."
do NMC=NMCStart,NMCEnd,step !Average over MC configurations
   !Loads the data
   inquire(iolength=reclen) rawData(1)
   open(unit=NMC,access='direct',form='unformatted',recl=reclen)
   do x=1,nx*ny*nz*nt !In each site
      read(unit=NMC,rec=x) rawData(x) 
   end do
   close(NMC)

   !Adds to the average array
   average = average + rawData
   
   !Starts computations for binned error estimator
   binIndex = NMC/(numConf/Nbins) !The bin number to which this configuration belongs

   !The binned data average over all bins, excluding the current one
   do m=1,Nbins
      if (m .ne. binIndex) then
         binnedData(:,m) = binnedData(:,m) + rawData
      end if
   end do
   
end do

!Now we normalize the data
average=average/dble(numConf)
binnedData=binnedData/dble(numConf-numConf/Nbins)

!And we compute the errors

do m=1,Nbins
   do x=1,nx*ny*nz*nt
      errors(x) = errors(x) + (average(x)-binnedData(x,m))**2
   end do
end do

errors = errors*(Nbins-1)/Nbins

print *, "Saving files..."
open(unit=1,file="StatisticalAverage.dat",form="unformatted",access="direct",recl=reclen)
open(unit=2,file="StatisticalError.dat",form="unformatted",access="direct",recl=reclen)
do x=1,nx*ny*nz*nt
   write(1,rec=x) average(x)
   write(2,rec=x) errors(x)
end do
close(1)
close(2)

deallocate(average, rawData, errors, binnedData)


print*, "Files saved and vectors deallocated."
print*, "All jobs done. See you. o/"

contains

   subroutine readParameters
      character(len=50) :: argNx, argNy, argNz, argNt, argNMCStartStep, argNMCEndStep, argNMCStepSize, argNbins
      integer, parameter :: minNumberParameters = 8 !Change this if input changes
      

      if (COMMAND_ARGUMENT_COUNT() .lt. minNumberParameters) then
         print*, "It is mandatory to pass at least 8 parameters as input in the format:"
         print*, "nx ny nz nt MCStartStep MCEndStep MCStepSize Nbins"
         print*, "Input files must be in the running folder and named fort.MCStepNumber"
         print*, "Because of the above, MCStartStep must be greater than 6 (to avoid conflict with stdin and stdout files)"
         print*, "Tip: You may want to create simlinks in the running folder to do this"
         print*, "Exiting now"
         call EXIT(1)
      else
         call GET_COMMAND_ARGUMENT(1,argNx)
         call GET_COMMAND_ARGUMENT(2,argNy)
         call GET_COMMAND_ARGUMENT(3,argNz)
         call GET_COMMAND_ARGUMENT(4,argNt)
         call GET_COMMAND_ARGUMENT(5,argNMCStartStep)
         call GET_COMMAND_ARGUMENT(6,argNMCEndStep)
         call GET_COMMAND_ARGUMENT(7,argNMCStepSize)
         call GET_COMMAND_ARGUMENT(8,argNbins)
         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt
         read(argNMCStartStep,*) NMCStart
         read(argNMCEndStep,*) NMCEnd
         read(argNMCStepSize,*) step
         read(argNbins,*) Nbins
      end if
   end subroutine

end program
