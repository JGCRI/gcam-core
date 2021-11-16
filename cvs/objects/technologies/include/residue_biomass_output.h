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


#if !defined( __RESIDUEBIOMASSOUTPUT_H )
#define __RESIDUEBIOMASSOUTPUT_H    // prevent multiple includes

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class PointSetCurve;
class ALandAllocatorItem;

/*!
 * \ingroup objects::biomass
 * \brief A class to output residue biomass supply to the energy
 *        market
 * \details This class contains a set of routines that implement
 *          output of residue biomass supply to an energy market. This object
 *          works for both ag and non-agricultural technologies. If the parameter
 *          mErosCtrl is not specified on input, then this object will work with
 *          non agricultural technologies. NOTE if mErosCtrl is specified for a 
 *          non ag technologies an error will result.
 *
 *
 *   <b>XML specification for ResidueBiomassOutput</b>
 *   - XML name: \c residue-biomass
 *   - Contained by: Technology
 *   - Parsing inherited from class: None.
 *   - Attributes: none
 *   - Elements:
 *   - \c curve-exponent ResidueBiomassOutput::mCostCurve.get/setCurveExponent()
 *   - \c eros-ctrl ResidueBiomassOutput::mErosCtrl
 *   - \c harvest-index ResidueBiomassOutput::mHarvestIndex
 *   - \c mass-conversion ResidueBiomassOutput::mMassConversion
 *   - \c mass-to-energy ResidueBiomassOutput::mMassToEnergy
 *   - \c mid-price ResidueBiomassOutput::mCostCurve.get/setMidprice()
 *
 * \author Kevin Walker
 * \todo Implement XMLDB derived class output so that maximum supply can be written out
 * \todo Figure out way to check for invalid input of mErosCtrl for non ag technology (need to check for existance of land allocator (easy) and existance of specified leaf)
 * \date $ Date $
 * \version $ Revision $
 */
class ResidueBiomassOutput : public IOutput
{
public :
   typedef IOutput parent;

    ResidueBiomassOutput( const std::string& sectorName = std::string() );
    virtual ~ResidueBiomassOutput(void);
    virtual const std::string& getXMLReportingName() const;
    virtual const std::string& getXMLName() const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual IOutput::OutputList calcPhysicalOutput( const double aPrimaryOutput,
                                                    const std::string& aRegionName,
                                                    const ICaptureComponent* aCaptureComponent,
                                                    const int aPeriod ) const;

    virtual ResidueBiomassOutput* clone( void ) const;

    virtual void completeInit( const std::string& aSectorName, const std::string& aRegionName,
                               const IInfo* aTechInfo, const bool aIsTechOperating );

    virtual double getEmissionsPerOutput( const std::string& aGHGName, const int aPeriod ) const;

    virtual const std::string& getName( ) const { return mName; }

    virtual double getPhysicalOutput( const int aPeriod ) const;

    virtual double getValue( const std::string& aRegionName, const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const;
    
    virtual std::string getOutputUnits( const std::string& aRegionName ) const;

    static const std::string& getXMLNameStatic( );

    virtual void initCalc( const std::string& aRegionName, const std::string& aSectorName, const int aPeriod );

    virtual bool isSameType( const std::string& aType ) const { return getXMLNameStatic().compare( aType ) == 0; }

    virtual void postCalc( const std::string& aRegionName, const int aPeriod );

    virtual void scaleCoefficient( const double aScaler );
    virtual void sendLandAllocator( const ILandAllocator* aLandAllocator, const std::string& aName );
    virtual void setName( const std::string& sectorName ) { mName = sectorName; }
    virtual void setPhysicalOutput( const double aPrimaryOutput, const std::string& aRegionName,
                                    ICaptureComponent* aCaptureComponent, const int aPeriod );

    virtual void setCurrencyOutput( const std::string& aRegionName,  const double aOutput, const int aPeriod ) { }
    virtual double getCurrencyOutput( const int aPeriod ) const { return 0; }
    
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    
    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );

    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IOutput* aPreviousInput,
                                   const IOutput* aNextInput );

protected :
    typedef objects::TechVintageVector<Value> value_vector_type;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IOutput,

        //! Physical output by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "physical-output", mPhysicalOutputs, value_vector_type ),

        /*!
        * Name of the secondary output. Corresponds to a market for this good
        * and a supply sector which supplies this good as its primary output.
        */
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! CO2 emissions coefficient cached from the marketplace.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "co2-coef", mCachedCO2Coef, Value ),

        //! The harvest index
        DEFINE_VARIABLE( SIMPLE, "harvest-index", mHarvestIndex, double ),

        //! The mass per unit area of biomass to be retained to prevent erosion
        DEFINE_VARIABLE( SIMPLE, "eros-ctrl", mErosCtrl, double ),

        //! The energy to mass conversion of the crop
        DEFINE_VARIABLE( SIMPLE, "mass-conversion", mMassConversion, double ),
        
        //! Water content of residue
        DEFINE_VARIABLE( SIMPLE, "water-content", mWaterContent, double ),

        //! The mass to energy conversion of the crop residue
        DEFINE_VARIABLE( SIMPLE, "mass-to-energy", mMassToEnergy, double ),

        //! Piece-wise linear cost curve 
        DEFINE_VARIABLE( CONTAINER | NOT_PARSABLE, "fract-harvested", mCostCurve, PointSetCurve* )
    )

    //! Weak pointer to the land leaf which corresponds to this biomass output
    //! used to save time finding it over and over
    ALandAllocatorItem* mProductLeaf;
    
    void copy( const ResidueBiomassOutput& aOther );
};

#endif   // __RESIDUEBIOMASSOUTPUT_H

