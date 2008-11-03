#ifndef _CARBON_BOX_MODEL_H_
#define _CARBON_BOX_MODEL_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
 * \file carbon_box_model.h
 * \ingroup Objects
 * \brief CarbonBoxModel class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "emissions/include/icarbon_calc.h"
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/time_vector.h"
#include "ccarbon_model/include/icarbon_container.h"
#include "ccarbon_model/include/carbon_model_utils.h"

class EnvironmentalInfo;
class CarbonBox;
class ICarbonContainer;

/*!
 * \brief Class that contains the carbon box model.
 * \details This class encapsulates the carbon model model for a land leaf.
 *          It contains many carbon boxes which contain transfers to other boxes.
 *          Additionally the box model can accept/make transfers from/to other carbon
 *          containers.
 */
class CarbonBoxModel: public ICarbonCalc,
                      public ICarbonContainer
                      { 

public:
    //! default constructor for CarbonBoxModel
    CarbonBoxModel();
    
    //! copy constructor for CarbonBoxModel
    CarbonBoxModel( const CarbonBoxModel& aCarbonBoxModel );    
    
    //! default destructor
    ~CarbonBoxModel();
    
    //! virtual copy constructor
    virtual CarbonBoxModel* clone() const;
    
    //! debug atmosphere calculation function 
    void doAtmosphereCalculation( int aYear );
    
    //! obtain the key from environmentalInfo
    int getKey() const;
    
    //! retrieve the address of a Box with a given name
    ICarbonContainer* getBoxByName( const std::string& aName ) const;
    
    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;
    static const std::string& getXMLNameStatic();

    // ICarbonCalc Interface
    virtual void completeInit( int aKey );
    virtual void initLandUseHistory( const LandUseHistory* aHistory,
                                     const double aShare );
    virtual void calc( const int aYear );
    virtual void calcLandUseChange( const int aYear, FlowType aFlowType );
    virtual double getNetLandUseChangeEmission( const int aYear ) const;
    virtual double getNetTerrestrial( const int aYear ) const;
    virtual void setTotalLandUse( const double aLandUse,
                                  const int aPeriod );
    virtual void setLandUseValue( const int aYear );
    virtual double getPotentialAboveGroundCarbon( const int aYear ) const;
    virtual void setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod );
    virtual double getPotentialBelowGroundCarbon( const int aYear ) const;
    virtual void setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod );

    // These methods are used in the simple carbon calculator
    virtual double getActualAboveGroundCarbonDensity( const int aYear ) const { return 0;};
    virtual void setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
        const int aPeriod ){};
    virtual double getActualBelowGroundCarbonDensity( const int aYear ) const{ return 0;};
    virtual void setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
        const int aPeriod ){};
    virtual void setMatureAge( const int aMatureAge ) {};    

    //ICarbonContainer
    virtual void doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                              const int aYear );
    virtual void addDependencies( DependencyFinder& aDepFinder ) const;
    virtual void acceptTransfer( double aCarbonValue, const int aYear,
                                 const BoxType aBoxType );
    virtual void addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType );
    std::vector<std::string> getBoxNames();
    virtual EnvironmentalInfo* getEnvironmentalInfo() const;
    virtual std::vector<CarbonBox*> getCarbonBoxes() const;
    
private:
    void setCurrentStock( const int aYear );
    void addBox( std::auto_ptr<CarbonBox> aCarbonBox );
    //! Vector of pointers to CarbonBoxes contained in this carbon box model.
    std::vector<CarbonBox*> mCarbonBoxes;
    //! EnvironmentalInfo object that contains various information.
    std::auto_ptr<EnvironmentalInfo> mEnvironmentalInfo;
    /*!
     * \brief A vector of booleans per year which represent whether the year has been
     *        calculated.
     */
    objects::YearVector<bool> mCalculated;
    objects::YearVector<bool> mCalculatedLUC;
    //! Above ground carbon content.
    objects::YearVector<double> mAboveGroundCarbon;
    //! Below ground carbon content.
    objects::YearVector<double> mBelowGroundCarbon;
    objects::YearVector<double> mAtmDeductionCondition;
    //! Map of names to pointers to boxes
    std::map< const std::string, ICarbonContainer* > mNamesToBoxes;

    void printGraph( const std::string& fileName ) const;
    void printGraphOneFile( const std::string& fileName,
                            const int aPeriod ) const;
    void duplicateNameWarning( std::string aName ) const;
    //! Vector of ACarbonFlow pointers that represent transfer to this box model's child boxes.
    boost::ptr_list<ACarbonFlow> mCarbonFlows;
};

#endif // _CARBON_BOX_MODEL_H_
