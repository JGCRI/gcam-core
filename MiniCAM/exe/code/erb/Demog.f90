!   Added correction factors to match scenarios  1/14/97 hmp
!   Fixed, read-in pop data for EPA study 7/26/93 (skip calcs.)
!*********************************************************************

      SUBROUTINE DemogMC(mERB,ZLM,WAgeMC,DemoStatMC)

!*********************************************************************
!
!          --  THE DEMOGRAPHICS MODULE --
!
! This module provides information regarding age and gender of regional 
! populations.  regional populations are divided into age groups based
! on the time step of the model.  
!
!  INTEGER INPUTS: NSTEP, NAGE
!
!  REAL INPUTS: 
!
!  REAL OUTPUTS:
!
!  SUBROUTINES CALLED: NONE
!
!  CODED BY
!        MARSHALL WISE
!        MAY, 1991
!
!  Modified to compute working age population and fraction of 
!  Population less than 15 and over 65  9-19-91  hmp
!  Modified version for MiniCAM 4/12/95 hmp
!
!*********************************************************************

      INCLUDE 'DEMOG.COM'                                                
      SAVE /DEMOG1/
      COMMON/NRegions/NLP,NNLP, REGNAMES
 
      PARAMETER( &
               nYear = 103)     !  Number of years
          
      Integer mERB             !  Time period from the ERB

      Real*4 MIGGCLM,MIGGLM,MIGLM,MIGGM,MIGM
      Real*4 CS10(2)                 !  Cumulative survival to age 10

      Real*4 &
            DemoStat(10,NLDPMax,nMp), &!  Demographic statistics  
            WAge(2,NLDPMax,nMp),       &!  working age population 
            TotCFert(NLDPMax,nMp),     &!  Total Completed Fertility 
            InfMort(2,NLDPMax,nMp),    &!  Infant Mortality 
            DeathR(2,nYear),        &!  Annual Death Rates by Age 
            LifeExp(2,NLDPMax,nMp),    &!  Life Expectency  
            CumSurv(2,0:nYear,NLDPMax),&!  Cumulative Survival 
            DRp1000(2,NLDPMax,nMp),    &!  death rate per 1000 
            MigRate(2,NLDPMax,nMp),    &!  Migration rate per thousand 
            WtTCF(NLDPMax,nMp),       &!  Total completed fertility wtd by cohort 
            WtWTCF(nMp),            &!  world total completed fertility 
            wt(NLDPMax),               &!  weights for global demostat 
            wtg(2,NLDPMax),            &!  weights for global demostat by gender 
            wt1990(nAGEp,NLDPMax)     !  weights for age standardized death rates

!      Logical eprint
     
!  .  Demographic statistics are 1  Total Completed Fertility
!                                2  Infant Mortality
!                                3  Life Expectency 
!                                4  Life Expectency at age 1
!                                5  Life Expectency at age 10
!                                6  Death Rate / 1000
!                                7  Migration Rate / 1000 (+ => pop gain)
!                                8  Dependency ratios 1 youth 2 elderly
!
!  .  Scaling factors for death rate adjustment
!  .  Maximum impact in 2020 with linear scale from 1990 to 2020 and 
!  .  linear scale to .5 from 2020 to 2100

!      eprint = .false.

      
!	open(41,file='d:\mcpro\v4\demog.log')

      if(mERB .eq. 1) then
!	   open(30,file='d:\mcpro\v4\totpop.txt')
         write(17,*) ' Reg,Year,TCF ,     Inf Mor, Life Exp,',&
          ' LE at 1 ,LE at 10, Death Rt,  Migr Rate,  Depd RtY,',&
          'Depd RtOld , Depd Rt Tot ,births, deaths, migrants, total pop'
	end if

!  .  Use ZLM data values for 1975--they are read in
!  .  Use popdata base numbers for 1990--so don't use demong until mERB = 3
!  .  do need to adjust poplm numbers for period 2 so just do code at end

      if (mERB .eq. 1) return     !  use zlm data as read in for 1975

!  .  Note that base year data is for 1985 in pop1.csv.  So need to run
!  .  demog once for mERB = 2.

      if (mERB .eq. 2) then
         mlow = 1   
         mhi = 1
      else if (mERB .lt. 9) then
         mlow = 2 + (mERB - 3) * 3 !  correct for different initial period,etc
         mhi = mlow + 2 
      else if (mERB .eq. 9) then 
         mlow = 2 + (mERB - 3) * 3 !  correct for different initial period,etc
         mhi = mlow + 3            !  generate numbers for 2100
      end if   

      Do m = mlow, mhi      !  loop 3 times for each ERB period after 2
         WtWTCF(m) = 0.     !  initialize world weighted tcf
         WCBFems = 0.       !  initialize world women of child bearing age
         iwcbfems = 0
         popm(m) = 0
	   do ng = 1,2
	      popgm(ng,m) = 0.
	      dthgm(ng,m) = 0.
	      miggm(ng,m) = 0.
	      do na = 1, nAGEp
	         popgcm(ng,na,m) = 0.
	      end do
	   end do

         do L = 1, NLP
!	      eprint = .false.
!            if (L .eq. 1) eprint = .true.
!  .  .  .  Compute survival rates
!  .  .  .  Begin by computing period specific death rate by sex--SVGCLM

!  .  .  .  First interpolate death rates to get annual rates
!  .  .  .  Special treatment of 0-1 rates.  Assume that 90 percent of
!  .  .  .  all deaths in the first 5 years occur in the first year
!  .  .  .  Let 1-2 and 2-3 be 25 percent of remainder.  Then interpolate
!  .  .  .  all remaining data using five year death rates as mid point
!  .  .  .  estimates.

!           Build Annual Death rates to develop life expectency stats.

            Do nG = 1,2
               Per0_1 = .9     !  Fraction of 0-4 deaths in 0-1 age range
               DeathR(nG,1) = Per0_1 * DRL(ng,1,L,m) * 5
               InfMort(ng,l,m) = Deathr(nG,1)
               DeathR(nG,2) = (1. - Per0_1) * 5 * DRL(nG,1,L,m) / 4.
               DeathR(nG,3) = (1. - Per0_1) * 5 * DRL(nG,1,L,m) / 4.
               Do i = 1,4
                  DeathR(ng,i+3) = (.2 * i * DRL(ng,2,L,m)) +   &
                                  (1. - (.2 * i) * DeathR(ng,3))
               End do
               DeathR(nG,8) = DRL(nG,2,L,m)
               Do NA = 3, nagep
                  nyr = (NA * 5) - 2
                  DeathR(ng,nyr) = DRL(nG,NA,L,m)
                  Do i = 1,4
                     DeathR(ng,i+nyr-5) = (.2 * i * DRL(ng,NA,L,m)) +   &
                                      ((1. - (.2 * i)) * DRL(ng,NA-1,L,m))  ! SJS indicies were reversed
                  End do
               End do   
               
               do i = 1,nyear
                  DeathR(ng,i) = DeathR(ng,i) / 1000.
                  if (DeathR(ng,i) .gt. 1.0) DeathR(ng,i) = 1.0
               End do

!  .  .  .  .  Now compute life expectency                  
                  
               LifeExp(ng,L,M) = 0.
               CumSurv(ng,0,L) = 1.0
               CS10(ng) = 0.0
               do i = 1,nyear
                  CumSurv(ng,i,L) = CumSurv(ng,i-1,L)  &
                                    * (1. - DeathR(ng,i))
                  if(i .le. 10) CS10(ng) = CS10(ng) + CumSurv(ng,i,l)
                  LifeExp(ng,l,m) = LifeExp(ng,l,m) + CumSurv(ng,i,l)
               end do
            End do   !  End of ng loop
            DemoStat(2,l,m) = (InfMort(1,l,m) + InfMort(2,l,m)) / 2.
            DemoStat(3,l,m) = (LifeExp(1,l,m) + LifeExp(2,l,m)) / 2.
	      t1 = (LifeExp(1,l,m) - Cumsurv(1,1,L)) / CumSurv(1,1,L)
	      t2 = (LifeExp(2,l,m) - Cumsurv(2,1,L)) / CumSurv(2,1,L)
            DemoStat(4,l,m) = (t1 + t2) / 2.  
            t1 = (LifeExp(1,l,m) - CS10(1)) / CumSurv(1,10,L)
            t2 = (LifeExp(2,l,m) - CS10(2)) / CumSurv(2,10,L)
            DemoStat(5,l,m) = (t1 + t2) / 2. 

!  .  .  .  Compute new births by gender

            BRTHGLM(1,L,M) = 0. 
            BRTHGLM(2,L,M) = 0. 
            DO NA = 2,12
               BRTHGLM(1,L,M) = BRTHGLM(1,L,M) + FCLM(NA,L,M)  &
                        * POPGCLM(2,NA,L,M-1) * GCLM(M,L)	! SJS corrected mistake in first index
               BRTHGLM(2,L,M) = BRTHGLM(2,L,M) + FCLM(NA,L,M)  &
                        * POPGCLM(2,NA,L,M-1) * (1.0 - GCLM(M,L))	! SJS corrected mistake in first index
            END DO 
            POPGCLM(1,1,L,M) = BRTHGLM(1,L,M)
            POPGCLM(2,1,L,M) = BRTHGLM(2,L,M)

!  .  .  .  Compute new population figures (in 1000S) by gender, age, and region
       
            DO NG=1,2
	         dthbab = DeathR(nG,1) +  &
                    .8 * (1. - DeathR(nG,1)) * DeathR(nG,2) + &
                    .6 * (1. - DeathR(nG,1)) * (1. - DeathR(nG,2)) * &
                          DeathR(nG,3) + &
                    .4 * (1. - DeathR(nG,1)) * (1. - DeathR(nG,2)) * &
                         (1. - DeathR(nG,3)) * DeathR(nG,4) + &
                    .2 * (1. - DeathR(nG,1)) * (1. - DeathR(nG,2)) * &
                         (1. - DeathR(nG,3)) * (1. - DeathR(nG,4)) * &
                          DeathR(nG,5)
  	         survbab = 1. - dthbab
               POPGCLM(nG,1,L,M) = survbab * BRTHGLM(ng,L,M)
	         DTHGCLM(nG,1,L,M) = (1 - survbab) * BRTHGLM(ng,L,M)
!               if(eprint)write(41,'(2f10.0)')
!     &            POPGCLM(nG,1,L,M),DTHGCLM(nG,1,L,M)
               DO NA=2,NAGEp - 1
                  DTHGCLM(ng,na,L,M) = POPGCLM(NG,NA-1,L,M-1)  &
                                     * (1. -  SVGCLM(NG,NA-1,L,M)) 
                  MIGGCLM(ng,na,L,M) = POPGCLM(NG,NA-1,L,M-1)  &
                                     * XMIG(NG,NA-1,L,M)
                  POPGCLM(NG,NA,L,M) = POPGCLM(NG,NA-1,L,M-1)  &
                                       + MIGGCLM(ng,na,L,M) &
                                       - DTHGCLM(ng,na,L,M)
!	         if(eprint)write(41,'(2i5,3f10.0)')ng,m,
!     &             popgclm(ng,na-1,l,m-1),miggclm(ng,na,l,m),
!     &             dthgclm(ng,na,l,m)
               END DO
               DTHGCLM(ng,nagep,L,M) = POPGCLM(NG,NAGEp-1,L,M-1)  &
                                    * (1. - SVGCLM(NG,NAGEp-1,L,M)) &
                                    + POPGCLM(NG,NAGEp,L,M-1)  &
                                    * (1. - SVGCLM(NG,NAGEp,L,M))
               MIGGCLM(ng,nagep,L,M) = POPGCLM(NG,NAGEp-1,L,M-1)  &
                                    * XMIG(NG,NAGEp-1,L,M)  &
                                    + POPGCLM(NG,NAGEp,L,M-1)  &
                                    * XMIG(NG,NAGEp,L,M) 
               POPGCLM(NG,NAGEp,L,M) = POPGCLM(NG,NAGEp-1,L,M-1) &
                                    + POPGCLM(NG,NAGEp,L,M-1)  &
                                    + MIGGCLM(ng,nagep,L,M) &
                                    - DTHGCLM(ng,nagep,L,M)
            END DO

!           Compute total completed fertility

            TotCFert(l,m) = 0.
            WtTCF(l,m) = 0.
            CBFems = 0.
            icbfems = 0
            Do NA = 1,NAGE
               TotCFert(l,m) = TotCFert(l,m)  + FCLM(na,l,m)
               WtTCF(l,m) = WtTCF(l,m) + FCLM(na,l,m)  &
                            * POPGCLM(2,NA,L,M)
               WtWTCF(m) = WtWTCF(m) + FCLM(na,l,m)  &
                            * POPGCLM(2,NA,L,M)
               if(fclm(na,l,m) .gt. 0.) then
                  cbfems = cbfems + popgclm(2,na,l,m)
                  icbfems = icbfems + 1
                  Wcbfems = wcbfems + popgclm(2,na,l,m)
                  iwcbfems = iwcbfems + 1
               end if
            End Do      
            DemoStat(1,l,m) = TotCFert(l,m) ! Use unweighted total -- standard method for comparison with other values SJS

!           Compute dependency ratios for the Young and the Old seperately

!           Population subgroup computations
!           Age 0-14 for youth dependency group
!           Age 15 to 65 for labor supply computations.
!           Age 65 + for elderly dependency computations

            DO NG = 1,2
	         yng(ng,L,m) = 0.
	         do NA = 1,3
	            Yng(ng,L,m) = Yng(ng,L,m) + POPGCLM(NG,NA,L,m)
	         end do
               WAGE(NG,L,m) = 0.
               DO NA = 4,13
                  WAGE(NG,L,m) = WAGE(NG,L,m) + POPGCLM(NG,NA,L,m)
               END DO
	         old(ng,L,m) = 0.
	         do NA = 14,nagep
	            old(ng,L,m) = old(ng,L,m) + POPGCLM(NG,NA,L,m)
	         end do
            END DO

	      twage = wage(1,l,m) + wage(2,l,m)
	      demostat(8,l,m) = (Yng(1,l,m) + yng(2,l,m)) / twage
	      demostat(9,l,m) = (old(1,l,m) + old(2,l,m)) / twage
	      demostat(10,l,m) = demostat(8,l,m) + demostat(9,l,m)

!           write(30,*)'  wage',wage(1,l,mhi),wage(2,l,mhi)

     
!           Compute regional gender, regional and world population totals
      
            POPLM(L,M) = 0.
	      BRTHLM(L,M) = 0.
	      DTHLM(L,M) = 0.
	      MIGLM(L,M) = 0.
            DO NG=1,2
               POPGLM(NG,L,m) = 0.
	         DTHGLM(ng,L,m) = 0.
	         MIGGLM(NG,L,M) = 0.
               DO NA=1,NAGEp
                  POPGLM(NG,L,m) = POPGLM(NG,L,m)  &
                                + POPGCLM(NG,NA,L,m)
                  POPGCM(NG,NA,M) = POPGCM(NG,NA,M)  &
                                 + POPGCLM(NG,NA,L,m)
	            DTHGLM(NG,L,M) = DTHGLM(NG,L,M) &
                                + DTHGCLM(NG,NA,L,M)
                  MIGGLM(NG,L,M) = MIGGLM(NG,L,M) &
                                + MIGGCLM(NG,NA,L,M)
               END DO
               POPLM(L,m) = POPLM(L,m) + POPGLM(NG,L,m)
	         BRTHLM(l,m) = BRTHLM(l,m) + BRTHGLM(ng,l,m)
	         DTHLM(L,M) = DTHLM(L,M) + DTHGLM(NG,L,M)
               MIGLM(L,M) = MIGLM(L,M) + MIGGLM(NG,L,M)
               popgm(ng,m) = popgm(ng,m) + popglm(ng,l,m)  
	         BRTHGM(ng,m) = BRTHGM(ng,m) + BRTHGLM(ng,l,m)
	         DTHGM(NG,M) = DTHGM(NG,M) + DTHGLM(NG,L,M)
	         MIGGM(NG,M) = MIGGM(NG,M) + MIGGLM(NG,L,M)
            END DO
            popm(m) = popm(m) + poplm(l,m)
	      BRTHM(m) = BRTHM(m) + BRTHLM(l,m)
	      DTHM(m) = DTHM(m) + DTHLM(L,M)
	      MIGM(m) = MIGM(m) + MIGLM(L,M)

!           Compute weights for age standardized statistics.

            if (m .eq. 1) then
	         do na = 1,nagep
	            wt1990(na,l) =  &
                        (popgclm(1,na,l,m) + popgclm(2,na,l,m))  &
                        / poplm(l,m)
               end do
            end if     

!           Compute age standardized migration and death rates.
            
            do ng = 1,2
	         MigRate(ng,l,m) = 0.
               DRp1000(ng,l,m) = 0.
               do na = 1,nagep
                  MigRate(ng,l,m) = MigRate(ng,l,m)  &
                                 + xMig(ng,na,l,m) * wt1990(na,l)
	            DRp1000(ng,l,m) = DRp1000(ng,l,m)  &
                                 + DRL(ng,na,L,m) * wt1990(na,l)
	         end do
	      end do
            Demostat(7,l,m) = (1000./ 5.) *  &
                (MigRate(1,l,m) + MigRate(2,l,m)) / 2.
            Demostat(6,l,m) = (DRp1000(1,l,m) + DRp1000(2,l,m)) / 2.
           write(17,'(2(i4,","),10(f10.4,","),5(f10.0,","))')l,m,(DemoStat(i,l,m),i=1,10) &
                          	 ,BRTHLM(l,m), DTHLM(L,M), MIGLM(L,M), poplm(l,m)     ! SJS Add births, deaths, migrants, & total                

         End Do   !  region loop on L

!        Print out population data set.

	   myear = 1985 + 5 * m
! 	   write(30,"(i5,11(I10,','))")myear,(lp,lp=1,NLP)
	   do ng = 1,2
 	      do na = 1, Nagep
!	        Write(30,"(5x,11(f10.0,','))")(popgclm(ng,na,lp,m),
!     &                                       lp=1,NLP)
	      end do
!	      Write(30,"(5x,11(f10.0,','))")(POPGLM(NG,Lp,m),lp=1,NLP)
         end do
!	   Write(30,"(5x,11(f10.0,','))")(POPLM(Lp,m),lp=1,NLP)
!	   write(30,"(5x,11(f12.0,','))")Popm(m)

         do l = 1, NLP
            wt(l) = poplm(l,m) / popm(m)
            do ng = 1,2
               wtg(ng,l) = popglm(ng,l,m) / popgm(ng,m)
            end do
         end do      

!        Compute world level demographic statistics
!        Male and female go in rows 1 and 2 beyond regional
!        totals for all population in row 3 which also has tcf for the world

         do istat = 2,10
            DemoStat(istat,NNLP,m) = 0.
            do l = 1,NLP
               DemoStat(istat,NNLP,m) = DemoStat(istat,NNLP,m)   &
                     +  wt(l) * DemoStat(istat,l,m)
            end do
         end do      
         
         DemoStat(1,NNLP,m) = (iwcbfems * WtWTCF(m))  &
                                 / (Wcbfems * NLP)
         write(17,'(2(i4,","),10(f10.4,","))')NNLP,m,(DemoStat(i,NNLP,m),i=1,10)

      End do      !  period loop on M  3 passes for each period in ERB    

!  .  Just build cumulative data for mERB periods
!  .  For last period correct mhi to be 2095 rather than 2100

  100 if (mERB .eq. 9) mhi = mhi - 1

      Z(mERB) = 0.0
      
      do l = 1,NLP
         Z(mERB) = Z(mERB) + POPLM(l,mhi)
      end do   

!     move population from poplm to zlm compressing from 23 to 9 periods.

      do l = 1,NLP
         zlm(l,mERB) = poplm(l,mhi)
	   WAgeMC(1,L,mERB) = WAge(1,L,mhi)
	   WAgeMC(2,L,mERB) = WAge(2,L,mhi)
	   do indx = 1,10
	      DemoStatMC(indx,L,mERB) = DemoStat(indx,L,mhi)
	   end do
      end do
      
	WAgeMC(1,NLP+1,mERB) = 0.
	WAgeMC(2,NLP+1,mERB) = 0.

	do l = 1, NLP
	   WAgeMC(1,NLP+1,mERB) = WAgeMC(1,NLP+1,mERB)  &
                                  + WAgeMC(1,L,mERB)
	   WAgeMC(2,NLP+1,mERB) = WAgeMC(2,NLP+1,mERB)  &
                                  + WAgeMC(2,L,mERB)
      end do
  

      RETURN

      END
