      SUBROUTINE Elec_Gen
      
!***********************************************************************     

!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!***********************************************************************
!
!    Subroutine to print an output file that states implied efficiencies
!    for new electric generation capacity in each region
!
!	Steve Smith. 01/00
!
!***********************************************************************

	  INTEGER IFuel(NUP)
	  REAL*8 GenCap(NUP,NM)
      CHARACTER*7 FUELName(14)
            
      DATA (IFUEL(J),J=1,8)/1,2,3,7,8,9,10,11/

      FUELName = (/'Oil    ','Gas    ','Coal   ','Nuclear','Solar  ','Hydro  ', &
                   'Biomass','CoalCap','OilCap ','GasCap ','H2Fcell', &
                   'Fusion ','Wind   ','SWStor '/)
     
!	 CHARACTER*8 REGNAMES(NNLPMax)

        OPEN (1,FILE="elecgen.csv")

         WRITE (1,*) "Implied efficiencies for new electric generation capacity"
         WRITE (1,*)
         WRITE (1,*)
         WRITE (1,*) "Efficiency (%)"
         WRITE (1,*) " -- Assumes capacity from 3 periods ago expired"
         WRITE (1,*) " -- Note: First 2 periods speculative"
         WRITE (1,'(f6.0,a)')  -99999999,"' = generation requirements decreased'"
  
       Write (1,'("Region,Fuel,",9(I4,","))') (M*15+1960,M=2,NM)    
       DO I = 1,NNU+NStype	!Include advanced "Solar-type" tech's
        DO L=1,NL
         J = IFuel(I)
         IF (J .ne. 0) THEN
          DO M=2,NM
            CurrentGen = ESUILM(J,L,M)
            CurrentFuel = CurrentGen*GUILM(J,L,M)
            PrevGen = ESUILM(J,L,M-1)
            IF (M .gt. 3) THEN
              PrevGen = PrevGen - ESUILM(J,L,M-3)
            ELSE
              PrevGen = PrevGen - ESUILM(J,L,1)/3.
            END IF
            PastFuel = PrevGen*GUILM(J,L,M-1)	!Assumes all leftover gen has last period's ave eff, eff would actually be higher since old stuff was worse
            GenNew = CurrentGen - PrevGen
            
            Prevg = GUILM(J,L,M-1)
            IF (GenNew .gt. 0) GNew = (CurrentFuel-PastFuel)/GenNew
            
            DiffEff = 0	! use if zero          
            IF (GenNew .gt. 0) DiffEff = 1/GNew
            IF (GenNew .lt. 0) DiffEff = -99999999	! Flag that generation decreased
            GenCap(J,M) = DiffEff
          END DO
          Write (1,'(2(I3,","),8(f6.0,","),2(a,","))') &
              L,J,(GenCap(J,M)*100,M=2,NM),FUELName(J),REGNAMES(L)     
         END IF ! J
        END DO ! I=1,NUP
       END DO
     
         WRITE (1,*)
         WRITE (1,*)
         WRITE (1,*) "Maximum required efficiency(%)"
         WRITE (1,*) " -- Assumes all prev capacity still on line"

       Write (1,'("Region,Fuel,",9(I4,","))') (M*15+1960,M=2,NM)    
       DO I = 1,NNU+NStype	!Include advanced "Solar-type" tech's
        DO L=1,NL
        J = IFuel(I)
         IF (J .ne. 0) THEN
          DO M=2,NM
            PrevGen = ESUILM(J,L,M-1)
            CurrentGen = ESUILM(J,L,M)
            CurrentFuel = CurrentGen*GUILM(J,L,M)
            PastFuel = PrevGen*GUILM(J,L,M-1)
            Prevg = GUILM(J,L,M-1)
            GenNew = CurrentGen - PrevGen
            IF (GenNew .gt. 0) GNew = (CurrentFuel-PastFuel)/GenNew
            
            DiffEff = 0	! use if zero          
            IF (GenNew .gt. 0) DiffEff = 1/GNew
            IF (GenNew .lt. 0) DiffEff = -99999999	! Flag that generation decreased
            GenCap(J,M) = DiffEff
          END DO
           Write (1,'(2(I3,","),8(f6.0,","),2(a,","))') &
              L,J,(GenCap(J,M)*100,M=2,NM),FUELName(J),REGNAMES(L)     
         END IF ! J
        END DO ! I=1,NUP
       END DO
     
              WRITE (1,*)
         WRITE (1,*) "Capacity increase over previous period"
         WRITE (1,*) 

       Write (1,'("Region,Fuel,",9(I4,","))') (M*15+1960,M=2,NM)    
       DO I = 1,NNU+NStype	!Include advanced "Solar-type" tech's
        DO L=1,NL
        J = IFuel(I)
         IF (J .ne. 0) THEN
          DO M=2,NM
            PrevGen = ESUILM(J,L,M-1)
            CurrentGen = ESUILM(J,L,M)
            GenNew = CurrentGen - PrevGen 
            GenCap(J,M) = 0	! use if zero or negative          
            IF (PrevGen .ne. 0) GenCap(J,M) = GenNew/PrevGen
          END DO
           Write (1,'(2(I3,","),8(f6.0,","),2(a,","))') &
              L,J,(GenCap(J,M)*100,M=2,NM),FUELName(J),REGNAMES(L)     
         END IF ! J
        END DO ! I=1,NUP
       END DO
     
         WRITE (1,*)
         WRITE (1,*)
         WRITE (1,*) "Average Generation Efficiency"
         WRITE (1,*) 

       Write (1,'("Region,Fuel,",9(I4,","))') (M*15+1960,M=2,NM)    
       DO I = 1,NNU+NStype	!Include advanced "Solar-type" tech's
        DO L=1,NL
        J = IFuel(I)
         IF (J .ne. 0) THEN
           Write (1,'(2(I3,","),8(f6.0,","),2(a,","))') &
              L,J,(100/GUILM(J,L,M),M=2,NM),FUELName(J),REGNAMES(L)     
         END IF ! J
        END DO ! I=1,NUP
       END DO
     

      close(1)

      RETURN

      END

