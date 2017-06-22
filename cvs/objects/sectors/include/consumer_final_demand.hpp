#ifndef _CONSUMERFINALDEMAND_HPP_
#define _CONSUMERFINALDEMAND_HPP_
#if defined(_MSC_VER)
#pragam once
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
 * \file consumer_final_demand.hpp
 * \ingroup Objects
 * \brief Header file for ConsumerFinalDemand, a multicomponent demand
 *        sector.
 * \author Robert Link
 */

#include <memory>
#include <vector>

#include "sectors/include/afinal_demand.h"
#include "functions/include/idemandsystem.hpp"
#include "util/base/include/value.h"
#include "util/base/include/ivisitor.h"


/*!
 * \ingroup Objects
 * \brief A class implementing a multicomponent demand sector. 
 * \details A consumer final demand calculates the total demand for
 *          several interrelated goods.  By "interrelated" we mean
 *          that the demand for any one of these goods depends on the
 *          prices and possibly quantities of the other goods.  For
 *          example, you could have cross-price elasticities between
 *          the goods and a total budget constraint.
 *
 *          The actual demand calculations are implemented in an
 *          object implementing the IDemandSystem interface, so the
 *          function of this class is to collect the prices needed by
 *          the demand system, call the calcDemand() function as
 *          required, add the resulting demands to the relevant
 *          markets, and store the results for later output.
 */
class ConsumerFinalDemand : public AFinalDemand
{
public:
    //! Constructor
    ConsumerFinalDemand(void);

    //! Destructor
    virtual ~ConsumerFinalDemand(void) {}

    //! XML name for this class
    static const std::string &getXMLNameStatic();

    //! virtual function for XML name
    virtual const std::string &getXMLName() const {return getXMLNameStatic();}

    /*
     * AFinalDemand Interface
     */
    
    //! \copydoc AFinalDemand::setFinalDemand()
    virtual void setFinalDemand( const std::string &aRegionName,
                                 const Demographic *aDemographics,
                                 const GDP* aGDP,
                                 const int aPeriod );

    /*!
     * \brief Report the demand for a period 
     * \details The caller supplies a vector to write the demand
     *          values into.  The vector will be resized if necessary
     *          to hold the demand values.  The values returned will
     *          be converted to the units given by the
     *          getReportingUnits().
     */
    void getDemand( std::vector<double> &aOutDemand, int aPeriod ) const;

    /*!
     * \brief Get the reporting units for each of the demand components. 
     * \details The vector argument will be resized as necessary.
     *          Reporting units may be different from the units used
     *          internally for calculation.
     */
    void getReportingUnits( std::vector<std::string> &aOutUnits ) const;

    /*!
     * \brief Get the name of each of the demand components. 
     * \details The vector argument will be resized as necessary and
     *          filled in with the names of the demand components.
     */
    void getComponentNames( std::vector<std::string> &aOutComponents ) const;

    /*!
     * \brief Get the market price of the service 
     * \details It's not clear what this value is supposed to mean, particularly
     *          in the context of a multicomponent demand, where the different
     *          demand components may not even share a unit.  Currently, this
     *          return value isn't even used by the rest of the model.  (It's an
     *          SGM relic.)  Therefore, for this class we always return zero.
     * \date 2017-02-08
     */
    virtual double getWeightedEnergyPrice( const std::string &aRegionName,
                                           const int aPeriod ) const {return 0;}

    //! \copydoc AFinalDemand::completeInit()
    virtual void completeInit( const std::string &aRegionName,
                               const IInfo *aRegionInfo );

    //! \copydoc AFinalDemand::initCalc()
    virtual void initCalc( const std::string &aRegionName,
                           const GDP *aGDP,
                           const Demographic *aDemographics,
                           const int aPeriod );

    //! \copydoc AFinalDemand::csvOutputFile()
    virtual void csvOutputFile( const std::string &aRegionName ) const;

    //! \copydoc AFinalDemand::dbOutput()
    virtual void dbOutput( const std::string &aRegionName ) const;


    /*
     * INamed interface
     */
    //! \copydoc INamed::getName()
    virtual const std::string &getName() const {return mName;}

    /*
     * IParsable interface
     */
    //! \copydoc IParsable::XMLParse()
    virtual bool XMLParse( const xercesc::DOMNode *aNode );

    /*
     * IVisitable interface
     */
    //! \copydoc IVisitable::accept()
    virtual void accept( IVisitor *aVisitor, const int aPeriod ) const;


    /*
     * IRoundTrippableInterface 
     */
    //! \copydoc IRoundTrippable::toInputXML()
    virtual void toInputXML( std::ostream &aOut, Tabs *aTabs ) const;

    // Technically part of AFinalDemand interface, but thematically similar to
    // IRoundTrippable.
    //! \copydoc AFinalDemand::toDebugXML()
    virtual void toDebugXML( const int aPeriod,
                             std::ostream &aOut,
                             Tabs *aTabs ) const;
 
   

private:
    //! Pointer to the demand system used in the sector
    std::unique_ptr<IDemandSystem> mDemandSys;

    /*!
     * \brief Names of the goods consumed in each of the demand components. 
     * \details These should be given in the same order that they are given in
     *          the demand system object, as we don't do any matching between
     *          them.  These names will be passed as the first argument to
     *          Marketplace::addToDemand, so just the name of the good is
     *          required; the region is a separate argument.
     */
    std::vector<std::string> mSupplySectors;

    /*!
     * \brief Prescribed output values during calibration years 
     * \details Components will be indexed in the same order as the
     *          entries in mSupplySectors, and within each component
     *          they will be indexed by year.  One side effect of this
     *          is that during parsing mSupplySectors must be
     *          populated with a component's name before any base
     *          service values can be read for that component.
     *
     *          Note that the components are stored row-wise, while
     *          periods are stored column-wise.  This is the opposite
     *          of what we do in mDemand.  That is because
     *          XMLHelper::insertValueIntoVector() needs the values
     *          for each component to be contiguous.  For the rest of
     *          the functions here it is more convenient to index
     *          years in rows and components in columns.
     *
     *          Base service values are stored in the units used by
     *          their respective supply sectors.
     */
    std::vector<std::vector<double> > mBaseServices; 
        
    std::string mName;          //!< name of the sector

    /*!
     * \brief Demand values over the course of the run.
     *
     * \details These are stored as a two-dimensional matrix.  The
     *          first dimension is the period, and the second is the
     *          demand component (corresponding to the supply sectors
     *          above).  Values will be filled in in setFinalDemand().
     *
     *          Demand is stored in the units used by the supply
     *          sectors.  When we go to report these values in
     *          getDemand(), we pass the stored values to the
     *          reportDemand() method of the demand system, which will
     *          convert them to the units we want to report.  Why do
     *          we delegate that to the demand system?  It's because
     *          we can't know what unit conversions are appropriate
     *          without knowing what specific demand system is in
     *          use.
     *
     *          If the demand system is per-capita, then these are
     *          also per-capita values; otherwise they are totals.
     *          This is not ideal, but we don't have access to the
     *          population when the reporting happens, so if we want
     *          to output the values as per-capita, we have to store
     *          them that way.

     */
    std::vector<std::vector<double> > mDemand;

    /*!
     * \brief State holder for Marketplace::setDemand()
     */
    std::vector<double> mLastDemand;

    /*!
     * \brief Derived clas visitor accept
     */
    void acceptDerived( IVisitor* aVisitor, int aPeriod ) const;
};

#endif  // _CONSUMERFINALDEMAND_HPP_
