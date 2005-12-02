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
* \date $Date$
* \version $Revision$
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
class OutputContainer;

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
public:
	RegionCGE();
	~RegionCGE(); 
	static const std::string& getXMLNameStatic();
	virtual void completeInit();
	virtual void initCalc( const int period);
	virtual void calc( const int period, const bool aDoCalibrations );
	void updateMarketplace( const int period );
    void updateAllOutputContainers( const int period );
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void updateOutputContainer( OutputContainer* outputContainer, const int period ) const;
	void csvSGMGenFile( std::ostream& aFile ) const;
protected:
    const static std::string XML_NAME; //!< node name for toXML method.
	std::vector<FinalDemandSector*> finalDemandSector; //!< vector of pointers to supply sector objects
    std::vector<FactorSupply*> factorSupply; //!< vector of pointers to factor supply objects
	std::vector<OutputContainer*> outputContainers; //!< vector of output containers
    std::map<std::string,int> finalDemandSectorNameMap; //!< Map of FinalDemandSector name to integer position in vector. 
	std::map<std::string,int> factorSupplyNameMap; //!< Map of factor supply name to integer position in vector. 

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

