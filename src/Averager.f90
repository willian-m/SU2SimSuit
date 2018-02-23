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
complex*16, allocatable, dimension (:) :: average,rawData,errors
complex*16, allocatable, dimension (:,:) :: binnedData

call readParameters
numConf = (NMCEnd-NMCStart)/step


allocate( average(nx*ny*nz*nt) )
allocate( binnedData(nx*ny*nz*nt,Nbins))

average = dcmplx(0.d0,0.d0)

do NMC=NMCStart,NMCEnd,step !Average over MC configurations
   !Loads the data
   inquire(iolenght=reclen) rawData(1)
   open(unit=NMC,access='direct',form='unformatted',recl=reclen)
   do x=1,nx*ny*nz*nt+1 !In each site
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
      reErr = dble(average(x)-binnedData(x,m))**2
      imErr = dimag(average(x)-binnedData(x,m))**2
      errors(x) = errors(x)+dcmplx(reErr,imErr)
   end do
end do

errors = errors*(Nbins-1)/Nbins

open(unit=1,file="StatisticalAverage.dat",format="unformatted",access="direct",recl=reclen)
open(unit=2,file="StatisticalError.dat",format="unformatted",access="direct",recl=reclen)
do x=1,nx*ny*nz*nt
   write(1,rec=x) average(x)
   write(2,rec=x) average(x)
end do
close(1)
close(2)

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
         read(argNx,*) nx
         read(argNy,*) ny
         read(argNz,*) nz
         read(argNt,*) nt
         read(argMCStartStep,*) NMCStart
         read(argMCEndStep,*) NMCEnd
         read(argMCStepSize,*) step
      end if
   end subroutine

end program
