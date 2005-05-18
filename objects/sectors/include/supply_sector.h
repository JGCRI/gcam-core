#ifndef _SUPPLY_SECTOR_H_
#define _SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file supply_sector.h
* \ingroup Objects
* \brief The SupplySector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/
#include <string>
#include "sectors/include/sector.h"
class NationalAccount;

/*! 
* \ingroup Objects
* \brief This class represents a single supply sector.
* \author James Blackwood
*/
class SupplySector: public Sector
{
public:
	explicit SupplySector( const std::string regionNameIn );
    virtual ~SupplySector(){};
    static const std::string& getXMLNameStatic();
    double getOutput( const int aPeriod ) const;
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                          const int aPeriod ){}; // Passing demographic here is not good.
protected:
    virtual void setMarket();
    virtual bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node ); 
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
	virtual const std::string& getXMLName() const;
private:
	const static std::string XML_NAME; //!< node name for toXML methods	
};

#endif // _SUPPLY_SECTOR_H_
