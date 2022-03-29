#ifndef _TRAN_TECHNOLOGY_H_
#define _TRAN_TECHNOLOGY_H_
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
* \file tran_technology.h
* \ingroup Objects
* \brief The transportation technology class header file.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

#include "technologies/include/technology.h"

class GDP;
class IInfo;
/*! 
* \ingroup Objects
* \brief This transportation technology class is based on the MiniCAM description of technology.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

class TranTechnology : public Technology
{
    friend class XMLDBOutputter;
public:
    TranTechnology( const std::string& aName, const int aYear );
    TranTechnology();
    virtual TranTechnology* clone() const;
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();

    virtual void initCalc( const std::string& aRegionName,
        const std::string& aSectorName,
        const IInfo* aSubsectorInfo,
        const Demographic* aDemographics,
        PreviousPeriodInfo& aPrevPeriodInfo,
        const int aPeriod );

    virtual void production( const std::string& aRegionName,
        const std::string& aSectorName, 
        double aVariableDemand,
        double aFixedOutputScaleFactor,
        const GDP* aGDP,
        const int aPeriod );

    virtual void calcCost( const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod );

    virtual double getTotalGHGCost( const std::string& aRegionName, const std::string& aSectorName, 
        const int aPeriod ) const;

    virtual double calcSecondaryValue( const std::string& aRegionName,
        const int aPeriod ) const;

    virtual double getEnergyCost( const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod ) const;

    double getCalibrationOutput( const bool aHasRequiredInput,
        const std::string& aRequiredInput,
        const int aPeriod ) const;

    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;
    
    virtual void doInterpolations( const Technology* aPrevTech, const Technology* aNextTech );
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! Vehicle load factor.
        DEFINE_VARIABLE( SIMPLE, "loadFactor", mLoadFactor, double )
    )
    
    void copy( const TranTechnology& aOther );

    double getTotalInputCost( const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod ) const;

    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    double getNonEnergyCost( const std::string& aRegionName,
        const std::string& aSectorName,
        const int aPeriod ) const;
};

#endif // _TRAN_TECHNOLOGY_H_
