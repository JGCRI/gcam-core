!***********************************************************************
!
      SUBROUTINE SYNFUELS
!
!***********************************************************************
!
!                     -- THE SYNFUEL PRODUCTION MODULE --
!
!
! CODED BY:
!     Marshall Wise 3/97                    
!     Separate technologies for producing synoil and syngas from 
!     coal and from biomass
!
!     Sonny Kim 5/99
!     Add natural gas to liquids technology.
!     Rewrite to loop over primary fuels
!
!     Steve Smith 8/01
!     Largely re-written synfuel sections.
!     This routine sets synfuel shares and prices only.
!     Routine SynAdjust (below) now sets absolute quantities and adjusts supply & demand
!
!***********************************************************************
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!     +-----------------------------------------------------------+
!     !   COMPUTE SYNFUEL CONTRIBUTION TO REFINABLE OIL AND GAS   !
!     +-----------------------------------------------------------+
!
      T = (M-1)*NJUMP
      SynShareJILM(:,:,L,M) = 0
      
!     LOOP OVER SYNFUEL TYPE; LIQUID AND GAS
      DO I=1,NSYN
        SynShareJILM(I,I,L,M) = 1.0	! Non-normalized share for non-syn (fossil) mode
        
!       LOOP OVER ENERGY RESOURCES FOR SYNFUEL PRODUCTION
!       NATURAL GAS, COAL AND BIOMASS
        DO J=2,INBMASS

!        Skip if both = natural gas
            IF(J.EQ.INGAS .AND. I.EQ.2) GO TO 10

!           Compute non-energy costs
            H=XNTERP(HCILT(J,I,L,1),HCILT(J,I,L,2),HCILT(J,I,L,3),T)
!
!           Compute price of synfuel by adding energy and non-energy costs
!           but first determine penalty for carbon emissions.         
!        
            IF (J.EQ.INGAS .OR. J.EQ.INCOAL) THEN
               TSYN =  TXUILM(J,L,M)  !Add carbon tax to input
               
! Default -- no sequestration
! Only use sequestration option for coal.
! No possibility for sequestration for gas to liquids (since can't take out any more carbon!). sjs - 09/01
	           S1=0.0
	           S2=1.0
	           
              IF (J.EQ.INCOAL) THEN
!              Test share between carbon realeased and sequestered 
!              Make capital cost penalty for removal equal to the carbon tax
!              implied in the electric power plants sequestration capital cost -- 1/5/98 

! New version. 09/01 - sjs
! Extra capital costs for disposal
! The HUILM are the capital costs per unit of electric output. Dividing by GUILM
! converts these to costs per unit input, which is what is needed here.
! Then multiplying by the synfuel conversion efficiency gives the cost per output
! of synfuels

    		   CPENLTY= (HUILM(JUCSCRUB,L,M)-HUILM(J,L,M)) &
                         /(COI(J)/1000.0)/GUILM(J,L,M) * GCI(J,I)			
                         
     
      ! Check here to make sure that is not negative, which is possible since these
      ! are input separately.
      IF (CPENLTY .lt. 0) THEN
           CPENLTY = 0.1*HUILM(J,L,M)/(COI(J)/1000.0)*GUILM(J,L,M)*GCI(J,I)	! set to some nominal amount
           IF (MODL .le. 2) THEN	! Only print error message for first run or so
              MsgStr = "Warning: Costs for sequest. vs. conven. elec gen inconsistent."
              Call MCLog(1,MsgStr,M,L,J,HUILM(JUCSCRUB,L,M)-HUILM(J,L,M))
           END IF
      END IF

! The carbon tax is in $/GJ
! The disposal costs are also in $/GJ, which are translated (through TXUILM0) 
! from $/ton, which varies by fuel

! Can only sequester difference between coal and final product carbon content
               SeqFractMax = (COI(INCOAL)-COI(I))/COI(INCOAL)

! Assume disposal costs are proportional to amount sequestered -- so multiply by 
! the maximum fraction a plant can sequester. 
! Then add carbon tax to remainder.
    
               CSTDISP=(CARBDISP(L,M)*SeqFractMax + CPENLTY)*TXUILM0(J,L,M)/100.0
               CSEQ=CSTDISP + (1.0-SeqFractMax)*TSYN +.0001
               CORIG = TSYN + .00001
               RHOT=-4
               SUMM=0.0
               S1 = CSEQ**RHOT
               S2=CORIG**RHOT
               SUMM = S1 + S2
               S1=S1/SUMM
               S2=S2/SUMM
               
           IF ((MODL .le. 30.and.MODL .ge. 27).and.M.gt.33) &
            write (*,*) M,L, "Costs: ",CSeq, TSYN, H, CPENLTY*TXUILM0(J,L,M)/100.0, &
                             CARBDISP(L,M)*SeqFractMax*TXUILM0(J,L,M)/100.0,SeqFractMax

! Make sure haven't exceeded maximum fraction (in aggregate, for all plants) 
!  --- adjust for this.  sjs - 09/01      
               IF(S1 .GT. SeqFractMax) THEN
	              S1=SeqFractMax
	              S2=(1-SeqFractMax)
	           ENDIF

!              Check coal phaseout date
               IF(M.GT.ICARBOUT(L,1) .AND.ICARBOUT(L,1).NE.0) THEN
	              S1=1.0
	              S2=0.0
	              ISCRUB=1
	           ENDIF

!              If no carbon tax, zero out share of sequestered
	           IF(TXUILM(J,L,M).EQ.0.0 .AND. ISCRUB.EQ.0) THEN
	             S1=0.0
	             S2=1.0
               ENDIF
               
	           REMFRACP(1,L)= REMFRAC(1,L)*S1
               H = H + S1*CSTDISP
	           TSYN = TSYN*(1.0-REMFRACP(1,L))
	         ENDIF ! Coal branch
	        
            ELSE	! If not coal or gas, i.e., biomass
               TSYN=0.0
            ENDIF

            PCJILM(J,I,L,M) = (P(J,L,M)+TSYN)*GCI(J,I)+H 

IF ((MODL .le. 30.and.MODL .ge. 27).and.M.gt.33) &
    write (*,'(5I3," Syn prices: ",10f7.1)') M,L,MODL,I,J, PCJILM(J,I,L,M),P(J,L,M),TSYN,GCI(J,I),H,1.11111,&
    		CSTDISP,CSEQ,SeqFractMax,S1

            
!shk 3/3/99
!           Synfuels compete with refined fuel prices. The share of synfuel 
!           demand is dependent on refined prices. However, synfuel quantities 
!           are added to crude resources because markets exist for crude fuels
!           only. The price of crude fuel derived from synfuels is marked up 
!           again in the refinery section of the code. There may be an issue 
!           with this approach in regards to trade of crude oil. It has to be
!           clear what the price of synfuels represent. Is it a price at which 
!           it competes with refined fuels or a price at which it replaces 
!           crude resources.

            SynShareJILM(J,I,L,M) = (PCJILM(J,I,L,M)/(PJLM(I,L,M)+TXUILM(I,L,M)))**RCI(J,I)
! 									Add in carbon tax to denominator. sjs - 08/01

     10    CONTINUE	!        LOOP ONLY THROUGH LIQUID IF N.GAS TO LIQUID
     
     END DO !END SYNFUEL Feedstock ("J") LOOP

     ShareSUM = SUM(SynShareJILM(:,I,L,M))
     SynShareJILM(:,I,L,M) = SynShareJILM(:,I,L,M)/ShareSUM	! renormalize to get shares 
     
   END DO !END SYNFUEL SUBSECTOR ("I") LOOP


    RETURN
	END

 
!***********************************************************************
!
      SUBROUTINE SynAdjust
!
!***********************************************************************
!
! Separated from Synfuel routine -- 08/01 - sjs
!     So that calcuation of syn shares can be done before price calcuations 
!
!***********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!     Initialize refined energy supply vars' with total production
      ESRILM(INCOAL,L,M)=ESIL1M(INCOAL,L,M)

      ESIL1M(IBMASS,L,M) = AGSUP(INBMASS,L)
      ESIL(IBMASS,L) = ESIL1M(IBMASS,L,M) + ESIL2M(IBMASS,L,M)
      ESRILM(IBMASS,L,M) = ESIL(IBMASS,L) 


! Initialize Synfuel vars to zero
      SYNFUEL(:,:,L,M) = 0
      SYNINPUT(:,L,M) = 0
      SYNLoss(:,:,L,M) = 0
      
!
! New Synfuel calcuations. sjs - 08/01
! Now, all synfuels are used for domestic end-use consumption only
! Share calculated in synfuel routine is now the share of END USE domestic consumption 
! supplied by syn fuels
! If production + imports of crude oil are not sufficient, then must import feedstocks
!
! Even if global market for liquids and gases, synfuel production is never larger than
! domestic end-use consumption.
!
! Global gas and liquids markets are now for fossil fuels only. Same for these prices.
!
   DO I=1,NSYN
      DO J=2,INBMASS
        IF (.not. (J.eq.INGAS .and. I .eq. 2)) THEN  ! Skip gas to gas conversion
            SYNFUEL(J,I,L,M) = SUM(EDRIKL(I,:,L)) * SynShareJILM(J,I,L,M)
            SYNLoss(J,I,L,M) = SYNFUEL(J,I,L,M)*(GCI(J,I) - 1.) 	! = *GCI(J,I)*(1 - 1/GCI(J,I))
            SYNINPUT(J,L,M)  = SYNINPUT(J,L,M) + SYNFUEL(J,I,L,M)*GCI(J,I)
         END IF
       END DO
    END DO 

! Now add demand for synfuel inputs to "primary" energy demands
    DO I=2,INBMASS
      IF (I .ne. INBMASS) EDRIL(I,L) = EDRIL(I,L) + SYNINPUT(I,L,M)
      IF (I .eq. INBMASS) EDRIL(IBMASS,L) = EDRIL(IBMASS,L) + SYNINPUT(I,L,M)
    END DO 
 

!******************************************************************
!     Compute total refinable oil and gas supplies by adding liquid 
!     and gas synfuels from n. gas, coal and biomass.

      DO I=1,NSYN
         ESRILM(I,L,M) = ESIL(I,L) + SYNFUEL(2,I,L,M) + SYNFUEL(3,I,L,M) + SYNFUEL(4,I,L,M)  
      END DO

    RETURN
	END
