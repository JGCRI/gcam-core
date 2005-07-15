#ifndef _IINVESTOR_H_
#define _IINVESTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file iinvestor.h
* \ingroup Objects
* \brief The IInvestor interface file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>

class Tabs;
class Demographic;
class IInvestable;
class NationalAccount;

/*! 
* \ingroup Objects
* \brief This class represents an object which controls sector level investment
*        in new technologies.
* \author Josh Lurz
*/
class IInvestor
{
public:
	inline IInvestor();
	inline virtual ~IInvestor();
    virtual void completeInit( const std::string& aRegionName, const std::string& aSectorName ) = 0;
    virtual void XMLParse( const xercesc::DOMNode* node ) = 0; 
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const = 0;
    
    virtual double calcAndDistributeInvestment( std::vector<IInvestable*>& aInvestables,
                                                NationalAccount& aNationalAccount, 
                                                const Demographic* aDemographic,
                                                const int aPeriod ) = 0;
};

//! Inline constructor
IInvestor::IInvestor(){
}

//! Inline destructor
IInvestor::~IInvestor(){
}

#endif // _IINVESTOR_H_
