        implicit real*8 (a-h,o-z)
        Parameter (N_max=3300000)
        dimension a(N_max,3),b(N_max,3),v(3,3)
        character*8 names(N_max)
        nframes=10
        natoms=648
        atm_o=16.0
        atm_si=28.09
        atm_zr=91.22
        atm_u=238.04
        open(10,file='HISTORY',status='old')
        
        
  10    Format(A80)
  20    Format(A21,1x,I7)
  30    Format(A8,6x,I4,42x,F10.7) 
  40    Format(A2)
  50    Format(A3,1x,F5.1,1x,A8)
  60    Format(A9,1x,F7.2,1x,A1)
  70    Format(F6.2,1x,A2,6F9.4)
        read(10,10) xnoneed
        read(10,10) noneed
        
        DO i=1,nframes
         
          
          read(10,30) xnoneed,nstep,time
          
          Do ii=1,3
          read(10,*)(v(ii,k),k=1,3)
          enddo
          d1=v(1,1)
          d2=v(2,2)
          d3=v(3,3)
          x0=0.1812068722
          y0=0.7243275536
          z0=3.721142589

          DO j=1,natoms
           read(10,40) names(j)
           read(10,*) a(j,1),a(j,2),a(j,3)
           read(10,*)
          enddo
          call open_write_sav(20,'s',i)
       write(20,20) 'Number of particles =',natoms
       write(20,50) 'A =',1.0,'Angstrom'
       write(20,60) 'H0(1,1) =',v(1,1),'A'
       write(20,60) 'H0(1,2) =',v(1,2),'A'
       write(20,60) 'H0(1,3) =',v(1,3),'A'
       write(20,60) 'H0(2,1) =',v(2,1),'A'
       write(20,60) 'H0(2,2) =',v(2,2),'A'
       write(20,60) 'H0(2,3) =',v(2,3),'A'
       write(20,60) 'H0(3,1) =',v(3,1),'A'
       write(20,60) 'H0(3,2) =',v(3,2),'A'
       write(20,60) 'H0(3,3) =',v(3,3),'A'
           do j=1,natoms
            if(names(j).eq.'Si') then
             write(20,70)atm_si,'Si',a(j,1)/d1,a(j,2)/d2,a(j,3)/d3,0.d0,0.d0,0.d0
            else if(names(j).eq.'O')then
             write(20,70)atm_o,'O',a(j,1)/d1,a(j,2)/d2,a(j,3)/d3,0.d0,0.d0,0.d0
            else if(names(j).eq.'Zr')then
             write(20,70) atm_zr,'Zr',a(j,1)/d1,a(j,2)/d2,a(j,3)/d3,0.d0,0.d0,0.d0
            else if(names(j).eq.'U')then
            write(20,70) atm_u,'U',a(j,1)/d1,a(j,2)/d2,a(j,3)/d3,0.d0,0.d0,0.d0

        endif
        enddo

      
         
         close(20)
        enddo
          
          end

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




