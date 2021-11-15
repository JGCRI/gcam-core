#ifndef _NUKE_FUEL_TECHNOLOGY_H_
#define _NUKE_FUEL_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file nuke_fuel_technology.h
* \ingroup Objects
* \brief The nuclear fuel technology class header file.
* \author Sonny Kim
*/

#include "technologies/include/technology.h"

class GDP;

/*! 
* \ingroup Objects
* \brief This nuclear fuel technology class is based on the MiniCAM description
*        of technology but includes the cost of conversion, enrichment,
*        fabrication, interim storage, and geologic disposal.
* \author Sonny Kim
*/

class NukeFuelTechnology : public Technology
{
public:
    NukeFuelTechnology( const std::string& aName, const int aYear );
    NukeFuelTechnology();
    virtual NukeFuelTechnology* clone() const;
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );
    
    double getNonEnergyCost( const int aPeriod ) const;

	virtual void production( const std::string& aRegionName,const std::string& aSectorName, 
							 double aVariableDemand, double aFixedOutputScaleFactor, const GDP* aGDP,
							 const int aPeriod );

protected:
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    virtual double getTotalInputCost( const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const int aPeriod ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,
    
        //! name of secondary fertile material used for making nuclear fuel
        DEFINE_VARIABLE( SIMPLE, "fertileFuelName", fertileFuelName, std::string ),

        //! name of secondary fertile material used for breeding fissile material
        DEFINE_VARIABLE( SIMPLE, "blanketFuelName", blanketFuelName, std::string ),

        //! Unit Conversion factor, used to be fmult.
        DEFINE_VARIABLE( SIMPLE, "fMultiplier", mConversionFactor, double ),

        //! Ratio of blanket to fuel materials (kgBlanket/kgFuel)
        DEFINE_VARIABLE( SIMPLE, "blanketFuelRatio", blanketFuelRatio, double ),

        //! designed burnup of fuel associated with nuclear plant (MWd/kgHM)
        DEFINE_VARIABLE( SIMPLE, "burnup", burnup, double ),

        //! uranium ore conversion cost ($/kgU)
        DEFINE_VARIABLE( SIMPLE, "conversionCost", conversionCost, double ),

        //! fissile material enrichment (%)
        DEFINE_VARIABLE( SIMPLE, "enrichmentProd", enrichmentProd, double ),

        //! feed material enrichment (%)
        DEFINE_VARIABLE( SIMPLE, "enrichmentFeed", enrichmentFeed, double ),

        //! tail enrichment (%)
        DEFINE_VARIABLE( SIMPLE, "enrichmentTail", enrichmentTail, double ),

        //! uranium enrichment cost ($/SWU)
        DEFINE_VARIABLE( SIMPLE, "enrichmentCost", enrichmentCost, double ),

        //! primary fuel fuel fabrication cost ($/kgHM)
        DEFINE_VARIABLE( SIMPLE, "fabricationCost", fabricationCost, double ),

        //! blanket material fabrication cost ($/kgHM)
        DEFINE_VARIABLE( SIMPLE, "blanketFabCost", blanketFabCost, double ),

        //! interim storage cost of spent fuel ($/kgHM)
        DEFINE_VARIABLE( SIMPLE, "interimStorageCost", interimStorageCost, double ),

        //! cost of permenant waste disposal ($/kgHM)
        DEFINE_VARIABLE( SIMPLE, "geologicWasteDisposalCost", geologicWasteDisposalCost, double ),

        //! reprocessing cost of spent fuel ($/kgHM)
        DEFINE_VARIABLE( SIMPLE, "reprocessingCost", reprocessingCost, double ),

        //! State value for fertile fuel market necessary to use Marketplace::addToDemand
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "fertile-fuel-state", mLastFertileValue, Value ),

        //! State value for blanket fuel market necessary to use Marketplace::addToDemand
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "blanket-fuel-state", mLastBlanketValue, Value )
    )
    
    void copy( const NukeFuelTechnology& aOther );

	static double getSWValue( const double aWeightFraction );

	double getSWUperProduct() const;
	double getFeedProductRatio() const;
    double getInitialMass() const;
    double getFertileEfficiency( const int aPeriod ) const;
    double getBlanketEfficiency( const int aPeriod ) const;
};

#endif // _NUKE_FUEL_TECHNOLOGY_H_
