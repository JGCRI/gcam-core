#ifndef _REGION_CGE_H_
#define _REGION_CGE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file region_cge.h
* \ingroup Objects-SGM
* \brief The RegionCGE class header file.
* \author Sonny Kim
*/

#include <map>
#include <iosfwd>

// User headers
#include "containers/include/region.h"
#include "containers/include/national_account.h"

// Forward declare headers
class ProductionSector;
class FinalDemandSector;
class FactorSupply;
class IVisitor;
// TEMP
class SGMGenTable;
class Tabs;

/*! 
* \ingroup Objects-SGM
* \brief This derived Region class contains SGM specific information.
* \todo Document this class further.
* \author Sonny Kim
*/

class RegionCGE : public Region
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class InputOutputTable;
    friend class XMLDBOutputter;
public:
    RegionCGE();
    ~RegionCGE(); 
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const GlobalTechnologyDatabase* aGlobalTechDB );
    virtual void initCalc( const int period);
    virtual void postCalc( const int aPeriod );
    virtual void calc( const int period, const bool aDoCalibrations );
    virtual void updateMarketplace( const int period );
    virtual void updateAllOutputContainers( const int period );
    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual void csvSGMGenFile( std::ostream& aFile ) const;

protected:
    const static std::string XML_NAME; //!< node name for toXML method.
    std::vector<FinalDemandSector*> finalDemandSector; //!< vector of pointers to supply sector objects
    std::vector<FactorSupply*> factorSupply; //!< vector of pointers to factor supply objects
	std::vector<SGMGenTable*> mOutputContainers; //!< vector of output containers
    std::vector<NationalAccount*> mNationalAccounts; //!< vector of NationalAccounts, one for each period.

    typedef std::vector<FinalDemandSector*>::iterator FinalDemandSectorIterator;
    typedef std::vector<FinalDemandSector*>::const_iterator CFinalDemandSectorIterator;
    typedef std::vector<FactorSupply*>::iterator FactorSupplyIterator;
    typedef std::vector<FactorSupply*>::const_iterator CFactorSupplyIterator;

    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    void operate( const int period );
private:
    void createSGMGenTables();
    void clear();
};

#endif // _REGION_CGE_H_
