#ifndef _CAL_QUANTITY_TABULATOR_H_
#define _CAL_QUANTITY_TABULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responisbility for the use of this software.
 */

/*! 
* \file cal_quantity_tabulator.h
* \ingroup Objects
* \brief CalQuantityTabulator class header file.
* \author Josh Lurz
*/

#include "util/base/include/default_visitor.h"
#include <map>
#include <string>

/*! 
* \ingroup Objects
* \brief A visitor which tabulates the calibrated and fixed supplies of a
*        region.
* \details This visitor when passed to a Region will tabulate individually the
*          fixed and calibrated supplies of all goods in the region. It will
*          also keep track of whether there are exist variable supplies of each
*          good in the region. This should be used instead of
*          Sector::getCalOutput if secondary outputs need to be included in the
*          sums. The visitor should be used by passing this object as an
*          argument to the accept function, and then access the information
*          through the getSupplyInfo function.
*
* \author Josh Lurz
*/
class CalQuantityTabulator : public DefaultVisitor {
public:
    /*
     * \brief A structure containing calibration information for a single good.
     */
    struct CalInfo {
    public:
        //! Total calibrated supply.
        double mCalQuantity;

        //! Total fixed supply.
        double mFixedQuantity;

        //! Whether all Technologies are calibrated or fixed.
        bool mAllFixed;

        //! Constructor which initializes all variables.
        CalInfo(): mCalQuantity( 0 ), mFixedQuantity( 0 ), mAllFixed( true ){}
    };

    //! Type of the map containing good names and their associated calibration
    //! information.
    typedef std::map<std::string, CalInfo> CalInfoMap;

    CalQuantityTabulator();
    
    // Documentation for visitor methods is inherited.
    virtual void startVisitRegion( const Region* aRegion,
                                   const int aPeriod );
    
    virtual void startVisitResource( const AResource* aResource,
                                     const int aPeriod );

    virtual void startVisitSubsector( const Subsector* aSubsector,
                                      const int aPeriod );

    virtual void endVisitSubsector( const Subsector* aSubsector,
                                    const int aPeriod );

    virtual void startVisitTechnology( const technology* aTechnology,
                                       const int aPeriod );

    virtual void endVisitTechnology( const technology* aTechnology,
                                     const int aPeriod );
    
    virtual void startVisitOutput( const IOutput* aOutput,
                                   const int aPeriod );

    // Non visitor interface method.
    const CalInfoMap& getSupplyInfo() const;
private:
    //! Map of output name to a struct containing the amount of calibrated
    //! supply, fixed supply, and whether all technologies were calibrated or
    //! fixed.
    CalInfoMap mCalSupplies;

    //! Name of the Region the calibration and fixed outputs are currently being
    //! summed for.
    std::string mCurrentRegionName;

    //! Current primary output calibration or fixed value. This is set to -1 if
    //! the Technology is variable.
    double mCurrentOutput;

    //! State of the current Technology.
    enum CalibrationState {
        //! Variable output.
        eVariable,

        //! Calibrated output.
        eCalibrated,

        //! Fixed output.
        eFixed,

        //! Unknown status.
        eUnknown
    };

    //! Current technology calibration state.
    CalibrationState mTechState;

    //! Current subsector calibration state.
    CalibrationState mSubsectorState;
};

#endif // _CAL_QUANTITY_TABULATOR_H_
