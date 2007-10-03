#ifndef _CARBON_BOX_H_
#define _CARBON_BOX_H_
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
 * \brief An implementation of the ICarbonContainer interface.
 * \details An implementation  of the ICarbonContainer interface that is used
 *          by carbon box models and by the carbon box containers in the summer
 *          object.
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

    virtual void doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                              const int aYear );
    virtual void acceptTransfer( double aCarbonValue, const int aYear,
                                 const BoxType aBoxType );
    virtual void addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType );
    void addDependencies( DependencyFinder& aDepFinder ) const;

    virtual const std::string& getName() const;
	int getLucflowValue();
	
	ICarbonStock* getCarbonStock();

	BoxType getCarbonBoxType();
	
	const boost::ptr_list<ACarbonFlow>::const_iterator getCarbonBoxFlow( const std::string aTargetName );
	const boost::ptr_list<ACarbonFlow>::const_iterator carbonBoxFlowNotFound();

protected:
    //! Vector of ACarbonFlow pointers that represent transfer to other boxes.
    boost::ptr_list<ACarbonFlow> mCarbonFlows;
    //! Vector of ACarbonFlow pointers that are held after parsing until completeInit is called.
    boost::ptr_list<ACarbonFlow> mFlowsFromParentBoxModel;
	//! Vector of ACarbonFlow pointers that contain the LUC-flow-in
	boost::ptr_list<ACarbonFlow> mLucFlowInBox;

private:
    void internalAddFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow );
    void internalAddParentFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow );
    void assignEnum( const std::string& aTypeString );
    //! Carbon stock object.
    std::auto_ptr<ICarbonStock> mStock;
    //! Name of the carbon box.
    std::string mName;
    BoxType mType;
};

#endif // _CARBON_BOX_H_

