#ifndef _NO_CLIMATE_MODEL_H_
#define _NO_CLIMATE_MODEL_H_
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
* \file no_climate_model.h
* \ingroup Objects
* \brief The NoClimateModel class header file.
* \author Pralit Patel
*/

#include <string>
#include <boost/core/noncopyable.hpp>

#include "climate/include/iclimate_model.h"

/*! 
 * \ingroup Objects
 * \brief A dummy climate model that does nothing.
 * \details When users are running GCAM in a mode that will produce an incomplete
 *          set of emissions then running the climate model with those emissions would
 *          produce invalid results.  In such a set up it would be better to use this
 *          dummy climate model to be clear that no climate information will be available.
 * \author Pralit Patel
 */

class NoClimateModel: public IClimateModel {
public:
	//! Constructor.
    NoClimateModel();
	
	//! Destructor.
    virtual ~NoClimateModel();
    
    static const std::string& getXMLNameStatic();

    virtual const std::string& getXMLName() const;
    
	virtual void toDebugXML( const int period, std::ostream& out,
                             Tabs* tabs ) const;

	virtual void completeInit( const std::string& aScenarioName );

    virtual bool setEmissions( const std::string& aGasName,
                               const int aPeriod,
		                       const double aEmission );
	
    virtual bool setLUCEmissions( const std::string& aGasName,
							  const int aYear,
							  const double aEmission );

    virtual double getEmissions( const std::string& aGasName,
                                 const int aYear ) const;

    virtual IClimateModel::runModelStatus runModel();

    virtual IClimateModel::runModelStatus runModel( const int aYear );
    
    virtual double getConcentration( const std::string& aGasName,
                                     const int aYear ) const;

    virtual double getTemperature( const int aYear ) const;

    virtual double getForcing( const std::string& aGasName,
                               const int aYear ) const;

    virtual double getTotalForcing( const int aYear ) const;

    virtual double getNetTerrestrialUptake( const int aYear ) const;

    virtual double getNetOceanUptake( const int aYear ) const;

	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;
    
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IClimateModel
    )
};

#endif // _NO_CLIMATE_MODEL_H_
