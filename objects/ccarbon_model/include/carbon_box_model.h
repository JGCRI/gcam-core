#ifndef _CARBON_BOX_MODEL_H_
#define _CARBON_BOX_MODEL_H_
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
