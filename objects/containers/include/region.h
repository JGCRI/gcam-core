#ifndef _REGION_H_
#define _REGION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file region.h
* \ingroup Objects
* \brief The Region class header file.
* \author Sonny Kim
*/

#include <map>
#include <vector>
#include <memory>
#include <string>
#include <list>
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/summary.h"
#include <boost/noncopyable.hpp>

// Forward declarations.
class Demographic;
class Sector;
class Curve;
class GHGPolicy;
class GlobalTechnologyDatabase;
/*! 
* \ingroup Objects
* \brief This is an abstract base class for Regions.
*
* \author Sonny Kim
*/

class Region: public IVisitable, public IRoundTrippable, protected boost::noncopyable
{
    friend class XMLDBOutputter;
public:
    Region();
    virtual ~Region();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const GlobalTechnologyDatabase* aGlobalTechDB );
    const std::string& getName() const;
    virtual void calc( const int period, const bool doCalibrations ) = 0;
    
    virtual void initCalc( const int period ) = 0;
    
    virtual void postCalc( const int aPeriod ) = 0;

    virtual void csvOutputFile() const {};
    virtual void dbOutput( const std::list<std::string>& aPrimaryFuelList ) const {};
    virtual void initializeAgMarketPrices( const std::vector<double>& pricesIn ) {};
    virtual void updateSummary( const std::list<std::string>& aPrimaryFuelList, const int period ) {};
    virtual const Summary& getSummary( const int period ) const { static const Summary nullSummary; return nullSummary; };
    void setTax( const GHGPolicy* aTax );
    const Curve* getEmissionsQuantityCurve( const std::string& ghgName ) const;
    const Curve* getEmissionsPriceCurve( const std::string& ghgName ) const;

    virtual bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const { return true; };
    virtual void setCalSuppliesAndDemands( const int period ) {};
    virtual void initializeCalValues( const int period ) {};
    virtual bool setImpliedCalInputs( const int period ) { return false; };
    virtual int scaleCalInputs( const int period ) { return 0; };
    virtual void updateAllOutputContainers( const int period ) = 0;
    virtual void updateMarketplace( const int period ) {};

    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const {};
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual void csvSGMGenFile( std::ostream& aFile ) const {};
protected:
    std::string name; //!< Region name
    std::auto_ptr<Demographic> demographic; //!< Population object

    std::vector<Sector*> supplySector; //!< vector of pointers to supply sector objects
    std::vector<GHGPolicy*> mGhgPolicies; //!< vector of pointers to ghg market objects, container for constraints and emissions

    typedef std::vector<Sector*>::iterator SectorIterator;
    typedef std::vector<Sector*>::const_iterator CSectorIterator;
    typedef std::vector<GHGPolicy*>::iterator GHGPolicyIterator;
    typedef std::vector<GHGPolicy*>::const_iterator CGHGPolicyIterator;

    virtual const std::string& getXMLName() const = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
private:
    void clear();
};

#endif // _REGION_H_
