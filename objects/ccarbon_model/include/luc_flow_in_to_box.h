#ifndef _LUC_FLOW_IN_TO_BOX_H_
#define _LUC_FLOW_IN_TO_BOX_H_
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
 * \file luc_flow_in_to_box.h
 * \ingroup Objects
 * \brief LUCFlowInToBox class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/acarbon_flow.h"

/*!
 * \brief A class which represents a flow from a CarbonBoxModel into one
 *        of its CarbonBoxes.
 * \details This class represents a flow of carbon due to positive land
 *          use change.  This flow should be from a CarbonBoxModel to one
 *          of its CarbonBoxes.
 *         
 *          This flow will multiply the value that should be transferred by a
 *          fraction read in from input.  This is used in case there is more
 *          than one type of a box (i.e vegetationA and vegetationB) and they
 *          should receieve different percentages.
 */
class LUCFlowInToBox : public ACarbonFlow {

public:
	LUCFlowInToBox();
    LUCFlowInToBox( ICarbonContainer* aTarget, const int aFraction );
    LUCFlowInToBox( ICarbonContainer* aTarget );
	LUCFlowInToBox( const LUCFlowInToBox& aLucFlowInToBox );
    virtual ~LUCFlowInToBox();
	virtual LUCFlowInToBox* clone() const;

    virtual bool XMLDerivedClassParse( const xercesc::DOMNode* aNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const std::map< const std::string, ICarbonContainer* > aNamesToBoxes,
                               int aKey );
    const std::string& getXMLName() const;

    virtual void transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType );

};

#endif // _LUC_FLOW_IN_TO_BOX_H_

