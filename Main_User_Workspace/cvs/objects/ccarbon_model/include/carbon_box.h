#ifndef _CARBON_BOX_H_
#define _CARBON_BOX_H_
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
 * \file carbon_box.h
 * \ingroup Objects
 * \brief CarbonBox class header file.
 * \author Jim Naslund and Ming Chang
 */


#include <boost/ptr_container/ptr_list.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/inamed.h"
#include "ccarbon_model/include/icarbon_container.h"
#include "ccarbon_model/include/carbon_box_model.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_stock.h"
#include <vector>

class ACarbonFlow;
class EnvironmentalInfo;
class DependencyFinder;
class LUCFlowOut;

typedef std::vector<CarbonBox*>::iterator CarbonBoxIterator;
typedef std::vector<CarbonBox*>::const_iterator CarbonBoxConstIterator;

/*!
 * \brief Carbon box class.
 * \details An implementation  of the ICarbonContainer interface that is used
 *          to hold each carbon box. This basic implementation represents a 
 *          normal carbon box (e.g. vegetation, soil, etc.)
 */
class CarbonBox : public ICarbonContainer,
                  public IRoundTrippable,
                  public IParsable,
                  public IVisitable,
                  public INamed
                  {

friend class ComplexCarbonDotPrinter;
friend class ComplexCarbonPrinter;
friend class XMLDBOutputter;

public:
    CarbonBox();
    CarbonBox( BoxType aType );
    CarbonBox( const CarbonBox& aCarbonBox );
    virtual ~CarbonBox();
    virtual CarbonBox* clone() const;
    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

    virtual bool matches( const BoxType aBoxType ) const;

    void completeInit( CarbonBoxModel& aParent );

    void setCurrentStock( const int aYear );

    const double getStock( const int aYear );

    virtual void doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                              const int aYear );
    virtual void acceptTransfer( double aCarbonValue, const int aYear,
                                 const BoxType aBoxType );
    virtual void addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType );
    void addDependencies( DependencyFinder& aDepFinder ) const;

    virtual const std::string& getName() const;
    
    ICarbonStock* getCarbonStock();

    BoxType getCarbonBoxType();

    const boost::ptr_list<ACarbonFlow>::const_iterator getCarbonBoxFlow( const std::string aTargetName );
    const boost::ptr_list<ACarbonFlow>::const_iterator carbonBoxFlowNotFound();

protected:
    //! Vector of ACarbonFlow pointers that represent transfer to other boxes.
    boost::ptr_list<ACarbonFlow> mCarbonFlows;

private:
    void internalAddFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow );
    void assignEnum( const std::string& aTypeString );
    //! Carbon stock object.
    std::auto_ptr<ICarbonStock> mStock;
    //! Name of the carbon box.
    std::string mName;
    BoxType mType;
};

#endif // _CARBON_BOX_H_

