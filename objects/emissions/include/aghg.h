#ifndef _AGHG_H_
#define _AGHG_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file aghg.h
* \ingroup Objects
* \brief The AGHG class header file.
* \author Sonny Kim
* \author Jim Naslund
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <memory>
#include <string>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

// Forward declarations
class GDP;
class IInfo;
class IOutput;
class Input;
class AEmissionsDriver;
class ICaptureComponent;

/*! 
 * \ingroup Objects
 * \brief The AGHG class describes a single gas.
 * \details The AGHG class describes a single gas with attributes of gas name,
 *          unit, emissions coefficients, and the calculated emissions.
 *
 *          Note that for non-CO2 GHGs, there are two methods of setting
 *          emissions. Through an emissions coefficient or a read-in input
 *          emissions for a base year (or years). These are mutually exclusive.
 *          The last one of these read in determines the method used.
 *
 *          Emissions emitted indirectly through use of technology are also
 *          calculated.
 * \author Sonny Kim, Marshall Wise, Steve Smith, Nick Fernandez, Jim Naslund
 */
class AGHG: public IVisitable, public IRoundTrippable
{ 
    friend class XMLDBOutputter;

public:
    //! Virtual Destructor.
    virtual ~AGHG();
    //! Clone operator.
    virtual AGHG* clone() const = 0;
    
    virtual void copyGHGParameters( const AGHG* prevGHG ) = 0;

    void XMLParse( const xercesc::DOMNode* tempnode );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();

    double getGHGValue( const Input* aInput, const std::string& aRegionName, const std::string& aProdName,
                        const int aPeriod ) const;
    /*! 
     * \brief Convert GHG tax and any storage costs into energy units using GHG
     *        coefficients and return the value or cost of the tax and storage
     *        for the GHG.
     * \details Applies taxes only if emissions occur. Emissions occur if there
     *          is a difference in the emissions coefficients.
     * \author Sonny Kim
     * \param aRegionName Name of the region for GHG
     * \param aFuelName Name of the fuel
     * \param aOutputs Vector of Technology outputs.
     * \param aEfficiency The efficiency of the technology this ghg emitted by.
     * \param aPeriod The period in which this calculation is occurring.
     * \param aSequestrationDevice The device responsible for capturing emissions.
     * \return Generalized cost or value of the GHG
     */
    virtual double getGHGValue( const std::string& aRegionName,
                                const std::string& aFuelName,
                                const std::vector<IOutput*>& aOutputs,
                                const double aEfficiency,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const = 0;

    virtual void calcEmission( const std::vector<Input*> aInputs, const std::string& aRegionName,
                               const std::string& aGoodName, const double aOutput, const int aPeriod );
    /*!
     * \brief Calculates emissions of GHG's
     * \details Emissions of these gases are equal to the emissions driver
     *          multiplied by the emissions coefficient (how much of the
     *          chemical forming the GHG is emitted per unit driver) multiplied
     *          by the control function (the extent to which regions are
     *          expected to put controls on end-of-pipe emissions- based on
     *          their pppGdp) multiplied by the result of the Marginal Abatement
     *          curve, and finally by an external read-in emissions Adjustment
     *          factor(if any). The function also sets the emissions coefficient
     *          if emissions are read in.  
     * \author Nick Fernandez, Steve Smith
     * \param aRegionName Name of the region for GHG
     * \param aFuelname The name of the fuel
     * \param aInput The amount of fuel sent out
     * \param aOutputs Vector of Technology outputs.
     * \param aGDP Regional GDP.
     * \param aSequestrationDevice The object potentially capturing emissions.
     * \param aPeriod The period in which this calculation is occurring.
     * \todo Emissions calc will not work properly with vintaging (base-year emissions will not work,
     *       and some thought needs to be given to how emissions controls should work)
     */
    virtual void calcEmission( const std::string& aRegionName,
                               const std::string& aFuelname,
                               const double aInput,
                               const std::vector<IOutput*>& aOutputs,
                               const GDP* aGDP,
                               ICaptureComponent* aSequestrationDevice,
                               const int aPeriod ) = 0;

    /*!
     * \brief Returns the name of ghg gas.
     * \return A string representing the name of the ghg gas.
     */
    virtual const std::string& getName() const = 0;
    double getEmission( const int aPeriod ) const;

    double getEmissFuel( const int aPeriod ) const;
    bool getEmissionsCoefInputStatus() const;
    void setEmissionsCoefInputStatus();
    std::string getGHGDriverName() const ;  // return a GHG driver name


    /*!
     * \brief Perform initializations that only need to be done once per period.
     * \param aRegionName Region name.
     * \param aFuelName Fuel name.
     * \param aLocalInfo The local information object.
     * \param aPeriod Model period.
     */
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aFuelName,
                           const IInfo* aLocalInfo,
                           const int aPeriod ) = 0;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    void setEmissionsDriver( std::auto_ptr<AEmissionsDriver>& aEmissionsDriver );

protected:

    AGHG();
    AGHG( const AGHG& other );
    AGHG& operator=( const AGHG& other );

    /*!
     * \brief Get the XML node name for output to XML.
     * \details This public function accesses the private constant string,
     *          XML_NAME. This way the tag is always consistent for both read-in
     *          and output and can be easily changed. This function may be
     *          virtual to be overridden by derived class pointers.
     * \author Jim Naslund
     * \return The constant XML_NAME.
     */
    virtual const std::string& getXMLName() const = 0;
    //! Unit of emissions
    std::string mEmissionsUnit; 

    double calcInputEmissions( const std::vector<Input*>& aInputs, const std::string& aRegionName, const int aPeriod ) const;

    std::vector<double> mEmissions; //!< emissions (calculated)
    std::vector<double> mEmissionsByFuel; //!< Emissions by primary fuel.

    std::auto_ptr<AEmissionsDriver> mEmissionsDriver; //!< emissions driver delegate

    /*!
     * \brief Parses any child nodes specific to derived classes
     * \details Method parses any input data from child nodes that are specific
     *          to the classes derived from this class.
     * \author Josh Lurz, Steve Smith
     * \param nodeName name of current node
     * \param curr pointer to the current node in the XML input tree
     * \return Whether any node was parsed.
     */
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    /*!
     * \brief Parses the name attribute of the GHG node.
     * \param nodeName The name to parse.
     */
    virtual void parseName( const std::string& aNameAttr ) = 0;
    /*!
     * \brief XML output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param out reference to the output stream
     * \param tabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    /*!
     * \brief XML debug output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param out reference to the output stream
     * \param tabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;

    virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
    double calcOutputCoef( const std::vector<IOutput*>& aOutputs, const int aPeriod ) const;
    void addEmissionsToMarket( const std::string& aRegionName, const int aPeriod );
    
private:
    void copy( const AGHG& other );

};

#endif // _AGHG_H_


