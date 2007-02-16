#ifndef _AEMISSIONS_COEF_H_
#define _AEMISSIONS_COEF_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*!
 * \file aemissions_coef.h
 * \ingroup Objects
 * \brief AEmissionsCoef header file.
 * \author Jim Naslund
 */

class IInfo;
class Tabs;

/*! 
 * \ingroup Objects
 * \brief An abstract class that encapsulates the emissions coefficient.
 * \details This class encapsulates the emissions coefficient.  Its subclasses
 *          determine how the emissions coefficient is calculated.
 *          This class provides empty implementations of setCoef, updateCoef,
 *          and initCalc because derived classes may not need to use them.
 * \author Jim Naslund
 */
class AEmissionsCoef{

public:
    AEmissionsCoef( const double aEmissionsCoef = 0 );
    virtual AEmissionsCoef* clone() const = 0;

    double getCoef() const;
    virtual void setCoef( const double aEmissionsCoef );
    virtual void updateCoef( const double aOutput );
    /*!
     * \brief Performs any initializations that only need to be done once per period.
     * \param aSubSectorInfo Pointer to the IInfo object
     * \param aName The name of the region
     */
    virtual void initCalc( const IInfo* aSubsectorInfo, const std::string& aName ) = 0;
    virtual double getEmissions( const double aOutput ) const;
    virtual double getInputEmissions() const;
    virtual double calcMaxCntrl( const double aFinalEmissCoef, const double aB,
                                 const double aMultiplier ) const;
    virtual bool needsCalcForAdjustment() const;
    bool getOverride() const;
    virtual void overrideCoef( const double aEmissionsCoef );

    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( std::ostream& out, Tabs* tabs ) const;
    /*!
     * \brief Get the XML node name for output to XML.
     * \details This public function accesses the XML_NAME of the coefficient.
     *          This way the tag is always consistent for both read-in and output and can be easily changed.
     *          This function may be virtual to be overridden by derived class pointers.
     * \author Jim Naslund
     * \return The constant XML_NAME.
     */
    virtual const std::string& getXMLName() const = 0;
    virtual double getXMLValue() const;

protected:
    double mEmissionsCoef;
    bool mOverridesFuturePeriods; //!< Whether this period's emissCoef overrides future period's emissCoef
  
};


#endif // _AEMISSIONS_COEF_H_
