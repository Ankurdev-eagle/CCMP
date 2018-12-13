      implicit real*8 (a-h,o-z)
      Parameter (N_max=8000000)
      dimension a(N_max,3),b(N_max,3),v(3,3)
      character*8 names(N_max)
      open (10,file='DEFECTS',status='old')
      am_o=16.0
      am_si=28.09
      am_u=238.04
      am_zr=91.22
      am_mg=24.30
      Ntot=81000
      nframes=526
      mk=0
  5     FORMAT(A8,4F6.1)
  10    FORMAT(A80)
  15    FORMAT(A21,1x,I7)
  16    FORMAT(A3,1x,F5.1,1x,A8)
  20    FORMAT(A9,1x,F7.2,1x,A1)
  30    FORMAT(F6.2,1x,A2,6F9.4)
  35    format(A10,I8,A17,I8,A13,I8)
  40    format(A8,I12)
  45    format(3F16.8)
      Do i=1,2
      read(10,10) xnoneed
      Enddo
       Do iii=1,nframes
             mk=mk+1
             read(10,10) xnoneed
             read(10,35) xnoneed,ndef,xnoneed,ni,xnoneed,nv
           Do i=1,3
             read(10,*) (v(i,j),j=1,3)
           Enddo
           d1=v(1,1)
           d2=v(2,2)
           d3=v(3,3)
       print*,mk
           Do i=1,(ni+nv)
             read(10,40) names(i),nindex
             read(10,*) a(i,1),a(i,2),a(i,3)
           Enddo
        call open_write_sav(20,'s',iii)
       write(20,15) 'Number of particles =',ndef
       write(20,16) 'A =',1.0,'Angstrom'
       write(20,20) 'H0(1,1) =',v(1,1),'A'
       write(20,20) 'H0(1,2) =',v(1,2),'A'
       write(20,20) 'H0(1,3) =',v(1,3),'A'
       write(20,20) 'H0(2,1) =',v(2,1),'A'
       write(20,20) 'H0(2,2) =',v(2,2),'A'
       write(20,20) 'H0(2,3) =',v(2,3),'A'
       write(20,20) 'H0(3,1) =',v(3,1),'A'
       write(20,20) 'H0(3,2) =',v(3,2),'A'
       write(20,20) 'H0(3,3) =',v(3,3),'A'
       Do i=1,ndef
!      print*, i
       IF(names(i).EQ.'v_U') THEN
      write(20,30) am_u,'U', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
          0.d0,0.d0,0.d0
       ELSE IF(names(i).EQ.'i_U') THEN
         write(20,30) am_u,'U', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
          0.d0,0.d0,0.d0
      else if(names(i).EQ.'v_Si') THEN
      write(20,30) am_si,'Si', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
         0.d0,0.d0,0.d0
       ELSE IF(names(i).EQ.'i_Si') THEN
         write(20,30) am_si,'Si', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
           0.d0,0.d0,0.d0
       else if(names(i).EQ.'v_O') THEN
      write(20,30) am_u,'O', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
          0.d0,0.d0,0.d0
       ELSE IF(names(i).EQ.'i_O') THEN
         write(20,30) am_u,'O', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
            0.d0,0.d0,0.d0
        else if(names(i).EQ.'v_Zr') THEN
      write(20,30) am_zr,'Zr', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
         0.d0,0.d0,0.d0
        ELSE IF(names(i).EQ.'i_Zr') THEN
         write(20,30) am_zr,'Zr', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
            0.d0,0.d0,0.d0
        else if(names(i).EQ.'v_Mg') THEN
      write(20,30) am_mg,'Mg', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
         0.d0,0.d0,0.d0
        ELSE IF(names(i).EQ.'i_Mg') THEN
         write(20,30) am_mg,'Mg', a(i,1)/d1,a(i,2)/d2,a(i,3)/d3,&
            0.d0,0.d0,0.d0


       ENDIF
       Enddo
       close(20)

       Enddo

       End
!     Subroutine open_write_sav opens a file for writing, configuration
!     filling in the extension with the no. of configuration
      subroutine open_write_sav(unit,file,ncfgsaved)
      integer unit,ncfgsaved
      character*(*) file,extension*10,name*32
      if (ncfgsaved.lt.10)then
      write(extension,1101) ncfgsaved
      else if (ncfgsaved.lt.100) then
      write(extension,1102) ncfgsaved
      else if (ncfgsaved.lt.1000) then
      write(extension,1103) ncfgsaved
      else if (ncfgsaved.lt.10000) then
      write(extension,1104) ncfgsaved
      endif
 1101  format('_',i1,'.cfg',3x)
 1102  format('_',i2,'.cfg',3x)
 1103  format('_',i3,'.cfg',2x)
 1104  format('_',i4,'.cfg',1x)
! We find the relevant part of file.
      I=1
      J=LEN(FILE)
      DO K=1,J
        IF(FILE(K:K).EQ.' ')THEN
          I=I+1
        ELSE
          GOTO 3000
        ENDIF
      ENDDO
 3000 CONTINUE
      DO K=J,1,-1
        IF(FILE(K:K).EQ.' ')THEN
          J=J-1
        ELSE
          GOTO 3010
        ENDIF
      ENDDO
 3010 CONTINUE
      name=file(I:J)//extension
      open (unit,file=name,STATUS='UNKNOWN')
      end

