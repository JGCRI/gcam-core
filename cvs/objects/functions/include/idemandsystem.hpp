#ifndef _IDEMANDSYSTEM_HPP_
#define _IDEMANDSYSTEM_HPP_
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
 * \file idemandsystem.h
 * \ingroup Objects
 * \brief Header file for IDemandSystem, an interface for demand systems
 * \author Robert Link
 */

#include <vector>
#include <string>

#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/inamed.h"

class Demographic;
class GDP;
class Value;

/*!
 * \ingroup Objects
 * \brief An interface for demand systems 
 * \details A demand system is a set of equations that jointly
 *          determines the demand for a collection of goods.  Demand
 *          is determined as a function of income and prices of all
 *          the goods in the system.  This differs from the simple
 *          demand functions used elsewhere in the model in that the
 *          prices and demands for the goods can affect one another.
 *          For example, there may be cross-price elasticities for
 *          complementary or substitute goods, or a budget constraint
 *          on total expenditures.
 *
 *          A key feature of a demand system is the number of goods 
 *          in the system.  We require this to be set at the creation of
 *          the object, rather than parsed from XML input.  The reason
 *          for this is that a demand system object will always be
 *          owned by a GCAM sector that will be responsible for
 *          collecting input prices from the GCAM markets and
 *          otherwise communicating with the rest of the model.  It's
 *          important for the sector and the demand system to agree on
 *          the number of goods, and we ensure that by having the
 *          parent object set that quantity when it creates the
 *          demands system object.
 *
 *          The second key feature of a demand system is its parameter
 *          list.  These *are* parsed from XML inputs, since the
 *          parent object doesn't need to know what they are.  The
 *          number of parameters need not (and generally won't) be
 *          equal to the number of goods.
 *
 *          A demand system may output either absolute or per capita
 *          values.  The base class holds a flag to track this
 *          property and provides a public method to report on it.
 *          The default, as set by the base class constructor, is per
 *          capita.  Subclasses may override this as required,
 *          including by parsing it from input XML.  However,
 *          they aren't required to do either.
 */
class IDemandSystem : public INamed, public IParsable, public IRoundTrippable {

public:

    // Constructor
    inline IDemandSystem( int angoods=1 ) : mNGoods(angoods), mPerCapita(true) {
        mDemandOutput.resize(angoods);
    }
    // Destructor
    virtual ~IDemandSystem(void) {}

    /* Interface for subclasses */
    
    /*!
     * \brief Calculate the demand as a function of its inputs. 
     * \details This function calculates the demand for the system.
     *          It should return a reference to the (mutable) member
     *          vector mDemandOutput, which should be presized to the
     *          correct number of components.  We do it this way to
     *          avoid having to allocate and return a new vector each
     *          time this function is called, as it could be called a
     *          lot. 
     * \note When we change over to using scratch spaces for holding
     *       internal state, we may need to rethink how we return this
     *       value, perhaps returning a handle to a section of the
     *       scratch space.  It's unclear, but this should do for
     *       now.
    */
    virtual void
        calcDemand( const std::string & aRegionName,
                    const Demographic & aDemographics,
                    const GDP & aGDP,
                    const std::vector<double> & aprices,
                    int aPeriod,
                    std::vector<double> & aDemandOutput) const = 0; 

    /*!
     * \brief Return the demand values to be written into the output database. 
     * \details This function exists to allow for the possibility of
     *          reporting demand in different units than we use for
     *          internal calculation.  Whereas calcDemand must return
     *          demand values in the same units used by the upstream
     *          sectors, these values may be in units more appropriate
     *          for analysis.
     *
     *          The parameter is used for both input and output.  On
     *          input it contains the demand originally calculated by
     *          the demand system (i.e., in GCAM's internal unit).
     *          The method should convert these values in place to the
     *          reporting unit.  The default implementation leaves the
     *          values untouched, and is therefore appropriate for
     *          cases where the reporting unit is the same as the
     *          internal unit.
     */
    virtual void reportDemand(std::vector<double> &aDemand) const {}

    /*!
     * \brief Provide unit strings for the demand components
     *
     * \details The caller supplies a string array, and this function
     *          fills it in.  The units can be different for each
     *          component, but they must be constant over time.
     */
    virtual void reportUnits(std::vector<std::string> &aUnits) const = 0;

    /*!
     * \brief Complete the demand system's initialization, prior to
     *        the start of the first model period. 
     * \details This method should be run as part of the containing
     *          sector's `completeInit()` method.
     * \param[in] aRegionName The name of the containing region.
     * \param[in] aSectorName The name of the containing sector.
     */
    virtual void completeInit( const std::string & aRegionName, const
                               std::string & aSectorName ) = 0;


    /* Other public member functions */
    
    /*!
     * \brief Return the number of demand components
     */
    int ngoods(void) const { return mNGoods; }
    /*!
     * \brief Indicate whether the demand output is per-capita
     */
    bool isPerCapita(void) const { return mPerCapita; }
    /*!
     * \brief Get the name used to refer to the class in XML files.
     */
    virtual const std::string &getXMLName(void) const = 0;

    /* Factory functions*/
    static bool isSubtype( const std::string &tpname );
    static IDemandSystem *create( const std::string &tpname );
    
protected:

    /*!
     * \brief Flag indicating whether or not output is per-capita. 
     * \details This value is protected (vice private) so that
     *          subclasses can set it during their setup (either on
     *          creation or during input parsing).  It should not be
     *          changed at any other time.  
     */
    bool mPerCapita;

    /*!
     * \brief Output vector for calcDemand. 
     * \details This vector holds the output of the calculations in
     *          calcDemand(), and that function returns a const
     *          reference to it.  This operation is intended to have
     *          return by value semantics; therefore, we do not
     *          provide any other way of accessing this value.
     */
    mutable std::vector<double> mDemandOutput;

private:

    /*!
     * \brief Number of goods in the demand system.
     * \details This value is set at construction and is read-only
     *          (via ngoods()) thereafter.
     */
    int mNGoods;

};

#endif /* _IDEMANDSYSTEM_HPP_ */    
