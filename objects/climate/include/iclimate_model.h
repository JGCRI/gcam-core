#ifndef _ICLIMATE_MODEL_H_
#define _ICLIMATE_MODEL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file iclimate_model.h
* \ingroup Objects
* \brief The IClimateModel interface header file.
* \author Josh Lurz
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"

class Tabs;
class IVisitor;

/*! 
* \ingroup Objects
* \brief An interface to a climate calculation model.
* \brief The climate model interface allows emissions of various gases to be
*        passed into a climate model and output from the climate model to be
*        printed to a database and text file. This interface is currently
*        limited and does not expose any output. Classes implementing this
*        interface must be able to accept emissions by gas and print output to a
*        database and CSV file. This output must include CO2 concentrations.
* \author Josh Lurz
*/

class IClimateModel: public IVisitable {
public:
	//! Constructor.
    inline IClimateModel();
	
	//! Destructor.
    virtual inline ~IClimateModel();
    
	/*! \brief Read in any data needed for this climate model.
	* \details Climate model is not required to read in data
    */
    virtual void XMLParse( const xercesc::DOMNode* node ) = 0;
    
	/*! \brief Write out any data needed for this climate model.
    */
	virtual void toInputXML( std::ostream& out,
                             Tabs* tabs ) const = 0;
    
	/*! \brief Write out debugging info for this climate model.
    */
	virtual void toDebugXML( const int period, std::ostream& out,
                             Tabs* tabs ) const = 0;

	/*! \brief Complete the initialization of the climate model.
	* \details Completes the initialization of the climate model, which must be
    *          done before emissions are added to the model.
	* \param aScenarioName The name of the current scenario.
    */
	virtual void completeInit( const std::string& aScenarioName ) = 0;

	/*! \brief Sets a level of emissions for a gas in a period.
	* \details This method will set the level of emissions for a gas which is
    *          used by the climate model. The level is set and will override any
    *          default or existing value. The period must be a model period, the
    *          model will perform any interpolation between periods or after the
    *          end period. The model will return false if the period is not
    *          valid. The gas must be known to the underlying model, and the
    *          model must return false if the gas is not known.
	* \pre The model must have its initialization method called before emissions
    *      can be set.
	* \param aGasName The name of the gas name for which to set emissions.
	* \param aPeriod The period in which to set emissions.
	* \param aEmission The level of emissions to set.
	* \return Whether the value was set correctly.
    */
    virtual bool setEmissions( const std::string& aGasName,
                               const int aPeriod,
		                       const double aEmission ) = 0;

	/*! \brief Run the climate model.
	* \details This method performs the climate calculations. It must be called
    *          after all emissions levels are set and before any output routines
    *          can be called. If this returns true it can be assumed that output
    *          from the model are valid.
	* \pre Emissions must be set before the model can be run.
	* \return Whether the model completed successfully.
    */
    virtual bool runModel() = 0;

    /*! \brief Returns the concentrations for a given gas in a given period from
    *          the climate model.
    * \details Queries the climate model for the concentration for a given gas
    *          and period and returns the value. If the climate model is
    *          unavailable or the gas is unknown the value returned is -1.
    * \param aGasName The name of the gas for which to return the concentration.
    * \param aYear The year for which to return the concentration.
    * \return The concentration for the period, -1 if the climate model is
    *         unavailable.
    */
    virtual double getConcentration( const std::string& aGasName,
                                     const int aYear ) const = 0;

    /*! \brief Returns the temperature in a given period from the climate model.
    * \details Queries the climate model for the temperature for a given period
    *          and returns the value. If the climate model is unavailable the
    *          value returned is -1.
    * \param aYear The year for which to return the temperature.
    * \return The temperature for the period, -1 if the climate model is
    *         unavailable.
    */
    virtual double getTemperature( const int aYear ) const = 0;

    /*! \brief Returns the forcing of a specific gas in a given period from the
    *          climate model.
    * \details Queries the climate model for the forcing for a given gas in a
    *          period and returns the value. If the climate model is unavailable
    *          the value returned is -1.
    * \param aGasName Name of the gas for which to get the forcing.
    * \param aYear The year for which to return the forcing.
    * \return The forcint for the period, -1 if the climate model is
    *         unavailable.
    */
    virtual double getForcing( const std::string& aGasName,
                               const int aYear ) const = 0;

    /*! \brief Returns the total forcing of all gases in a given period from the
    *          climate model.
    * \details Queries the climate model for the total forcing for all gases in
    *          a period and returns the value. If the climate model is
    *          unavailable the value returned is -1.
    * \param aYear The year for which to return the total forcing.
    * \return The forcing for the period, -1 if the climate model is
    *         unavailable.
    */
    virtual double getTotalForcing( const int aYear ) const = 0;

	/*! \brief Print the output of the climate model to a file.
	* \details Writes a subset of the output of the model to a file. The path to
    *          the file is currently hardcoded due to limitations in the output
    *          routines. This file is the same as all other CSV output is
    *          written to.
	* \pre The model must be run before output can be written.
    */
    virtual void printFileOutput() const = 0;

	/*! \brief Print the output of the climate model to a database
	* \details Writes a subset of the output of the model to a database.
	* \pre The model must be run before output can be written.
    */
    virtual void printDBOutput() const = 0;

    /*! \brief Update a visitor with information from the climate model.
    * \param aVisitor Vistor to update.
    * \param aPeriod Period for which to perform the update, -1 means all
    *        periods.
    * \pre The model must be run before the update can occur.
    */
	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;
};

// Inline definitions to avoid compiler warnings and errors.
IClimateModel::IClimateModel(){
}

IClimateModel::~IClimateModel(){
}

#endif // _ICLIMATE_MODEL_H_
