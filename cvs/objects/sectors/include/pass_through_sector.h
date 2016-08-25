#ifndef _PASS_THROUGH_SECTOR_H_
#define _PASS_THROUGH_SECTOR_H_
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
 * \file pass_through_sector.h
 * \ingroup Objects
 * \brief The PassThroughSector class header file.
 * \author Pralit Patel
 */
#include <string>
#include "sectors/include/supply_sector.h"
#include "containers/include/iactivity.h"

/*!
 * \ingroup Objects
 * \brief This class represents a pass-through supply sector.
 * \author Pralit Patel
 */
class PassThroughSector: public SupplySector
{
    friend class CalcFixedOutputActivity;
public:
    explicit PassThroughSector( const std::string& aRegionName );
    virtual ~PassThroughSector(){};
    static const std::string& getXMLNameStatic();

    virtual void completeInit( const IInfo* aRegionInfo,
                               ILandAllocator* aLandAllocator );


    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

protected:
    virtual double getFixedOutput( const int aPeriod ) const;

    virtual const std::string& getXMLName() const;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );

    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;

    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const;

private:
    void setFixedDemandsToMarket( const int aPeriod ) const;

    //! The appropriate sector name for which's marginal revenue should be used
    //! when calculating fixed output.
    std::string mMarginalRevenueSector;

    //! The market in which to find the marginal revenue sector.
    std::string mMarginalRevenueMarket;
};

/*!
 * \ingroup Objects
 * \brief An adaptor class to generically call the PassThroughSector to calculate
 *        and set it's fixed output.
 * \author Pralit Patel
 */
class CalcFixedOutputActivity : public IActivity
{
public:
    CalcFixedOutputActivity( const PassThroughSector* aSector );
    virtual ~CalcFixedOutputActivity();

    // IActivity methods
    virtual void calc( const int aPeriod );

    virtual void setStale();

    virtual std::string getDescription() const;

private:
    //! A weak reference to the sector that will do the work
    const PassThroughSector* mSector;
};

#endif // _PASS_THROUGH_SECTOR_H_

