        implicit none
        real*8 hdr(1), tpc(1,20)
        real*8 bmiss, getbmiss
        CHARACTER(len=180) filn
        CHARACTER(len=8)  SUBSET
        integer  iret, idate, ilev, j, tvflg, lubfi
        integer nargs, iargc
        real vtcd

        data lubfi /10/
        
        nargs = iargc()
        if (nargs .lt. 1) then
           print *, 'Usage: check_virtmp.x prepbufr-file'
           stop
        end if
        call getarg(1,filn)
        open(unit=lubfi,file=filn,form='unformatted')

        CALL OPENBF(LUBFI,'IN',LUBFI)
        call ufbqcd(lubfi,'VIRTMP',vtcd)
        tvflg = 1
        subset = ''
        iret = 0
        do while (iret == 0 .and. subset /= 'SFCSHP')
           CALL READMG(LUBFI,SUBSET,IDATE,IRET)
        end do


C  LOOP THROUGH THE INPUT MESSAGES - READ THE NEXT SUBSET
C  ------------------------------------------------------

        do while(iret .eq. 0) 
          CALL READSB(LUBFI,IRET)
          IF(IRET .eq. 0) then
            call ufbint(lubfi,hdr,1,1,ilev,'TYP')
            call ufbevn(lubfi,tpc,1,1,20,ilev,'TPC')
            do j = 1,20
               if(tpc(1,j) == vtcd) tvflg = 0
               if(tpc(1,j) >= bmiss) exit
            end do
            if (tvflg == 0) exit
          else
              subset = ''
              iret = 0
              do while (iret == 0 .and. subset /= 'SFCSHP')
                 CALL READMG(LUBFI,SUBSET,IDATE,IRET)
              end do
          endif
        end do
        if (tvflg == 0) then
           print *,'VIRTUAL'
        else
           print *,'DRY'
        end if

        stop
        end
