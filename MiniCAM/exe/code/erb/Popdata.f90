!**********************************************************************

      SUBROUTINE POPDATA(iunit)

!**********************************************************************
!
!  Subroutine to read in population data from an excel spread 
!    sheet based data set.
!    
!    
!
!   CODED BY:
!     MARSHALL WISE
!      JULY 26,1991


!   EXTRACTED FROM DEMOG0A 9/18/91 hmp
!   Fixed, read-in pop data for EPA study 7/26/93    cmn
!   Combined demographic rate and population versions 3/31/94 hmp
!   moved code to compute final rate data into input file 3/98 hmp 
!   Added ability to read 5 year rate data  3/98  hmp
!   Drop age from time to reach final rates  4/98  hmp
!   extend death rates to 100  4/98  hmp
!**********************************************************************

!  .  Include common blocks


      INCLUDE 'DEMOG.COM'                                                
      SAVE /DEMOG1/
      COMMON/NRegions/NLP,NNLP, REGNAMES
      CHARACTER*80 MsgStr	! Since not in 

       CHARACTER*4 RATE,CALR
      Character*80  PopOutFile  !  Population output data file
      Character*80  title       !  title of each section of data
!      Character*25 Popufile
      
!      Popufile = 'C:\minicam\input\pop3.csv'

!      Logical eprint
!     eprint = .false.
!	open(42,file='d:\mcpro\v4\popdata.log')

      RATE = 'RATE'
      CALR = 'CALR' 
      
!  .  Open input data set

!       OPEN(iunit,FILE=POPUFILE)

!  .  Determine whether population data set is in rate format or absolute
!  .  population format.  Rate type must have RATE in first four spaces of 
!  .  line and population data must have POPS in this position.

      READ (iunit,'(A4)')POPTYPE
      Read (iunit,*) PopOutFile
                                                 
      OPEN(17,file=PopOutFile,status='unknown')
      write(17,*)'population stats output file'
      
      
      IF (POPTYPE .EQ. RATE) THEN
!        This reads in the original demographic rate information    
!  .  .  Read in initial population data and rate information to develop
!  .  .  populations over time.
!  .  .  This data is for 1985 which is period 0 in the model so set M = 0
!  .  .  The data set has covers all 11 regions for version 0.0 so we need
!  .  .  to read all this data.  Use NLL = 11 instead of NL as read in.

         NAGE = 16
	   NAGE20 = 20
         NSTEP = 5
         NLL = 11
         M = 0

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)
         
!  .  .  Read in base period population data         

         DO NG=1,2
            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)

            DO L=1,NLL
               READ(iunit,*)IDUM1,(POPGCLM(NG,NA,L,M),NA=1,NAGE)
            END DO
         END DO

!  .  .  Read in initial and terminal age-specific annual fertility rates

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)

         DO L=1,NLL
            READ(iunit,*)IDUM1,(FCLM(NA,L,M),NA=1,NAGE)
         END DO

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)

         DO L=1,NLL
            READ(iunit,*)IDUM1,(FCLZ(NA,L),NA=1,NAGE)
         END DO

!  .  .  Read in initial and terminal death rates by gender, region, age group
!        Extend from age 78 to age 103 using linear interpolation.

         DO NG=1,2

            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)

            DO L = 1,NLL
               READ(iunit,*)IDUM1,(SVGCLM(NG,NA,L,M),NA=1,NAGE)
	         DO IAGE = 1,4
	            SVGCLM(NG,NAGE+IAGE,L,M) = SVGCLM(NG,NAGE+iage-1,L,M)  &
                                             * 2.33
	            if(SVGCLM(NG,NAGE+IAGE,L,M).gt. 1000.) then
	                SVGCLM(NG,NAGE+IAGE,L,M) = 1000.
	            end if
               end do
	         SVGCLM(NG,NAGEp,L,M) = 1000.
            END DO

            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)

            DO L=1,NLL
               READ(iunit,*)IDUM1,(SVGCLZ(NG,NA,L),NA=1,NAGE)
	         DO IAGE = 1,4
	            SVGCLZ(NG,NAGE+IAGE,L) = SVGCLZ(NG,NAGE+Iage-1,L)  &
                                             * 2.33
	            if(SVGCLZ(NG,NAGE+IAGE,L).gt. 1000.) then
	                SVGCLZ(NG,NAGE+IAGE,L) = 1000.
	            end if
	         end do
               SVGCLZ(NG,NAGEp,L) = 1000.
            END DO
         END DO

!  .  .  Read in male birth fractions by Year, region

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)

         DO L=1,NLL
            READ(iunit,*)IDUM1,(GCLM(nM,L),nM=1,nMp)
         END DO

!  .  .  Read in initial/terminal migration rates by gender, age group, region

         DO NG=1,2

            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)
  
            DO L=1,NLL
               READ(iunit,*)IDUM1,(XMIG(NG,NA,L,M),NA=1,NAGE)
            END DO

            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)

            DO L=1,NLL
               READ(iunit,*)IDUM1,(XMIGZ(NG,NA,L),NA=1,NAGE)
            END DO

         END DO

!  .  .  Read in time-to-terminal fertility rate by age group, region

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)

         DO L=1,NLL
            READ(iunit,*)IDUM1,TFCLZ(L)
         END DO

!  .  .  Read in time-to-terminal death rates by gender, age, region

         DO NG=1,2

            READ(iunit,*)
            READ(iunit,*)
            READ(iunit,*)

            DO L=1,NLL
               READ(iunit,*)IDUM1,TSVGCLZ(NG,L)
            END DO
         END DO

!  .  .  Read in time-to-terminal net migration rate by region

         READ(iunit,*)
         READ(iunit,*)
         READ(iunit,*)

         DO L=1,NLL
            READ(iunit,*)IDUM1,TXMIGZ(L)
         END DO
      
!  .  .  Compute net migration rates

         do m = 1,nMp
            do L = 1,NLP
!	         eprint = .false.
!	         if (L .eq. 1) eprint = .true.
               DO NG=1,2
                  DO NA=1,NAGE
                     IF ((nStep * M) .LT. TXMIGZ(L)) THEN
                        XXMIG(NG,NA,L) = (nStep * M) / TXMIGZ(L)
                     ELSE
                        XXMIG(NG,NA,L)=1.0
                     END IF
                     XMIG(NG,NA,L,M) = XMIG(NG,NA,L,0) *  &
                                       (1.0 - XXMIG(NG,NA,L)) +  &
                                       XMIGZ(NG,NA,L) * XXMIG(NG,NA,L)
!	               if (eprint)write(42,*)ng,m,xmig(ng,na,l,m)

!                    Convert annual net migration per thousand 
!                    to 5-year rate per thousand

                     XMIG(NG,NA,L,M) = 5.0 * XMIG(NG,NA,L,M) / 1000.0
!	               if (eprint)write(42,*)ng,m,xmig(ng,na,l,m)
                  END DO
               END DO  !  end of ng loop


!  .  .        Compute fertility rates

               DO NA=1,NAGE
                  IF ((nStep * M) .LT. TFCLZ(L)) THEN
                     FXCLM(NA,L) = (nStep * M) / TFCLZ(L)
                  ELSE
                     FXCLM(NA,L) = 1.0
                  END IF
                  FCLM(NA,L,M) = FCLM(NA,L,0) * (1.0 - FXCLM(NA,L))  &
                                 + FCLZ(NA,L) * FXCLM(NA,L)

!                 Convert annual fertility rates to 5-year rates per female

                  FCLM(NA,L,M) = 5.0 * FCLM(NA,L,M) / 1000.0
!                  if(eprint)write(42,'(2i3,f10.5)')na,m,fclm(na,l,m)
               END DO

!              Convert annual death rates to five year survival rates
!              first do linear interpolation between initial and terminal

               DO NG = 1,2
                  DO NA = 1,NAGE
                     IF ((nStep * M) .LT. TSVGCLZ(NG,L))  THEN
                        SVXGCLM(NG,NA,L) = (nStep * M)  &
                                           / TSVGCLZ(NG,L)
                     ELSE
                        SVXGCLM(NG,NA,L)=1.0
                     END IF
                     DRL(NG,NA,L,M) = SVGCLM(NG,NA,L,0)  &
                                      * (1.0 - SVXGCLM(NG,NA,L))  &
                                      + SVGCLZ(NG,NA,L)  &
                                      * SVXGCLM(NG,NA,L)

!  .  .  .     .     Convert annual death rates to 5-year survival probabilities

                     SVGCLM(NG,NA,L,M) = (1.0 - DRL(NG,NA,L,M)  &
                                         / 1000.0) ** 5  
                  end do                    
               END DO
            END DO   !   end of L loop
	   end do  !  end of m loop for converting rates

	ELSE IF (POPTYPE .EQ. CALR) THEN
         
!        read input file from popcalib for use in the MiniCAM.
!        first read adjusted population by age for 1990

         NAGE = 16
         NSTEP = 5
         NLL = 11
         M = 0

         read(iunit,*)title
	   do Lp = 1,NLP
            do ng = 1,2
	         read(iunit,*) &
                    iytemp,Lx,nx,(popgclm(ng,na,Lp,0),na=1,nagep)
	      end do
         end do

!        Now read survival rates by region, sex and age by period.

         read(iunit,*)title
	   do Lp = 1, NLP
	      do ng =1,2
	         do m = 1,nMp
	            read(iunit,*) &
                     iytemp,Lx,nx,(svgclm(ng,na,lp,m),na=1,nagep)
	            do na = 1,nagep
	               if(svgclm(ng,na,lp,m) .le. 0.0) then
	                  DRL(NG,NA,Lp,M) = 1000.
                     else	                 
	                  DRL(NG,NA,Lp,M) = 1000. - 1000.  &
                                          * svgclm(ng,na,lp,m) ** .2
	               end if
                  end do
	         end do
	      end do
         end do
	  
!        Now read survival rates by region, sex and age by period

         read(iunit,*)
	   do Lp = 1, NLP
	      do ng =1,2
	         do m = 1,nMp
	            read(iunit,*) &
                     iytemp,Lx,nx,(xmig(ng,na,lp,m),na=1,nagep)
	         end do
	      end do
         end do

!        Now read fertility rates by region and age by period

         read(iunit,*)
	   do Lp = 1, NLP
	      do m = 1,nMp
	         read(iunit,*) &
                  iytemp,Lx,(fclm(na,lp,m),na=1,12)
	      end do
         end do

!        read fraction of male births by region and period
 
         read(iunit,*)
	   do Lp = 1,NLP
	      read(iunit,*) &
               Lx,(gclm(m,Lp),m=1,nMp)
	   end do

      ELSE
		MsgStr = "Invalid Population Data Set"
        Call MCLog(1,MsgStr,0,0,0,0)

      STOP
      END IF

      RETURN
      END
