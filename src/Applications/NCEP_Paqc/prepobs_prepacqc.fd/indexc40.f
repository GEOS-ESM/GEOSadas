c$$$  subprogram documentation block
c
c subprogram: indexc40
c   Programmer: D. Keyser       Org: NP22       Date: 2012-05-08
c
c Abstract: Uses efficient sort algorithm to produce index sort list for a 40-character
c   array.  Does not rearrange the file.
c
c Program History Log:
c 1993-06-05  R  Kistler -- FORTRAN version of C-program
c 1993-07-15  P. Julian  -- Modified to sort 12-character array
c 1994-08-25  D. Keyser  -- Modified to sort 16-character array
c 1995-05-30  D. Keyser  -- Tests for < 2 elements in sort list, if so returns without
c                            sorting (but fills indx array)
c ????-??-??  P. M. Pauley (NRL) -- Size of carrin changed to character*24
c 2010-11-15  S. Bender  -- Size of carrin changed to character*40
c 2012-05-08  D. Keyser  -- Prepared for operational implementation
c             
c Usage:    call indexc40(n,carrin,indx)
c
c   Input argument list:
c     n        - Size of array to be sorted
c     carrin   - 40-character array to be sorted
c
c   Output argument list:
c     indx     - Array of pointers giving sort order of carrin in ascending order {e.g.,
c                carrin(indx(i)) is sorted in ascending order for original i = 1, ... ,n}
c
c Remarks: Called by main program.
c
c Attributes:
c   Language: FORTRAN 90
c   Machine:  NCEP WCOSS
c
c$$$
      subroutine indexc40(n,carrin,indx)

      implicit none

      integer      n              ! dimension of array to be sorted
     +,            j              ! do loop index, sort variable
     +,            i              ! sort variable
     +,            l              ! variable used to decide if sort is finished
     +,            ir             !           "                 "
     +,            indx(n)        ! pointer array
     +,            indxt          ! pointer used in sort

      character*40 carrin(n)      ! input array to be sorted
     +,            cc             ! character variable used in sort

c # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      do j = 1,n
        indx(j) = j
      enddo

c Must be > 1 element in sort list, else return
c ---------------------------------------------

      if(n.le.1)  return

      l = n/2 + 1
      ir = n

   33 continue
      if(l.gt.1) then
         l = l - 1
         indxt = indx(l)
         cc = carrin(indxt)
      else
         indxt = indx(ir)
         cc = carrin(indxt)
         indx(ir) = indx(1)
         ir = ir - 1
         if(ir.eq.1) then
            indx(1) = indxt
            return
         endif
      endif

      i = l
      j = l * 2

   30 continue
      if(j.le.ir)  then
        if(j.lt.ir)  then
          if(carrin(indx(j)).lt.carrin(indx(j+1)))  j = j + 1
        endif
        if(cc.lt.carrin(indx(j))) then
          indx(i) = indx(j)
          i = j
          j = j + i
        else
          j = ir + 1
        endif
      endif

      if(j.le.ir) go to 30
      indx(i) = indxt
      go to 33

      end

