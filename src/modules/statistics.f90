!=====THE STATISTICS MODULE======
!This module contains functions for computing statistical relavant
!quantities, such as averages, standard deviations and correlation times
!
!Many of the routines are based on the article of
!Alan D. Sokal, "Monte Carlo Methods in Statistical Mechanics: 
!Foundations and New Algorithms",
!in Book "Functional Integration - Basics and Applications"
!

!-----------------------------------------------------
!=====VARIABLES AND TYPES=====
!-----------------------------------------------------



!-----------------------------------------------------
!=====SUBROUTINES AND FUNCTIONS======
!-----------------------------------------------------
!



      module statistics
      implicit none

      contains


!=====COMPUTES AVERAGE
      real*8 function average(array,arraySize,startSlice,endSlice)
      integer, intent(in) :: arraySize,startSlice,endSlice
      real*8, dimension(arraySize), intent(in) :: array
      real*8 :: soma
      integer :: i

      soma = 0.d0
      do i=startSlice,endSlice
        soma = soma + array(i)
      end do
      average = soma/dble(endSlice-startSlice+1)
      end function average

!=====COMPUTES STANDARD DEVIATION
      real*8 function std(array,arraySize,startSlice,endSlice)
      integer, intent(in) :: arraySize,startSlice,endSlice
      real*8, dimension(arraySize), intent(in) :: array
      real*8 :: soma, avrg
      integer :: i

      soma = 0.d0
      avrg = average(array,arraySize,startSlice,endSlice)

      do i=startSlice,endSlice
        soma = soma + (array(i)-avrg)**2
      end do
      std = dsqrt(soma/dble(endSlice-startSlice+1))
      end function std

!=====COMPUTES CORRELATION FUNCTION
      subroutine corr_func(array,arraySize,startSlice,endSlice,corrFunc)
      integer, intent(in) :: arraySize,startSlice,endSlice
      real*8, dimension(arraySize), intent(in) :: array
      real*8, allocatable, intent(out), dimension(:) :: corrFunc
      real*8 :: soma, avrg, norm
      integer :: i,j, maxCorrFuncSize

      maxCorrFuncSize = (endSlice - startSlice)
      if (maxCorrFuncSize .le. 0) then
        print *, "endSlice should be greater than startSlice. Aborting correlation function computation!"
        stop
      else if (maxCorrFuncSize .lt. 100) then
        print *, "slice should be at least size 100. Received",maxCorrFuncSize,". Aborting correlation function computation!"
        stop
      else
        maxCorrFuncSize = 100
      end if

      if (.not. allocated(corrFunc) )then
        allocate(corrFunc(maxCorrFuncSize))
      else
        deallocate(corrFunc)
        allocate(corrFunc(maxCorrFuncSize))
      end if

      avrg = average(array,arraySize,startSlice,endSlice)

      do j=1,maxCorrFuncSize
        soma = 0.d0
        do i=startSlice,endSlice-j
            soma = soma + (array(i)-avrg)*(array(i+j-1)-avrg)
        end do
        corrFunc(j) = soma/(endSlice - j - startSlice + 1)

      end do


      if (corrFunc(1) .gt. 1.d-32) then
        norm = corrFunc(1)
        do j=1,maxCorrFuncSize
            corrFunc(j) = corrFunc(j)/norm
        end do
      end if

      end subroutine corr_func

!=====COMPUTES INTEGRATED CORRELATION TIME
      real*8 function corr_time(corrFunc)
      implicit none
      real*8,dimension(1000),intent(in) :: corrFunc
      integer :: i,m = 0
      real*8 :: tau = 1.d0


      !print *, size(corrFunc)
      do while (dble(m) .lt. 8.d0*tau)
        tau = 1.d0
        do i=1,m
            if (i .le. size(corrFunc) ) then
                tau = tau + corrFunc(i)
            end if
        end do
        m = m + 1
      end do

      corr_time = tau
      end function

!=====COMPUTES ERROR ESTIMATOR BY BINNING
      real*8 function binningError(array,arraySize,startSlice,endSlice,cTime)
      integer, intent(in) :: arraySize,startSlice,endSlice
      real*8, intent(in) :: cTime
      real*8, dimension(arraySize), intent(in) :: array
      real*8, allocatable, dimension (:) :: binnedData
      real*8 :: soma, avrg,aux
      integer :: i, nBins, binSize

      soma = 0.d0
      avrg = average(array,arraySize,startSlice,endSlice)

      !How many bins we will build?
      nBins = int((endSlice-startSlice)/ctime - 1.0)
      !Which size each bin will have?
      binSize = (endSlice-startSlice)/nBins

      !Allocate vector of the binned data
      allocate(binnedData(nBins))
      !Fill the vector of the binned data
      do i=1,nBins
        binnedData(i) = average(array,arraySize,startSlice+(i-1)*binSize,startSlice+i*binSize)
      end do

      !Estimates the error
      do i=1,nBins
        soma = soma + (binnedData(i)-avrg)**2
      end do
      binningError = dsqrt(soma)/dble(nBins)/dble(nBins-1)

      end function binningError
      end module statistics
