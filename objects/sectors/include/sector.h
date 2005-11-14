#ifndef _SECTOR_H_
#define _SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file sector.h
* \ingroup Objects
* \brief The Sector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <map>
#include <memory>
#include <iosfwd>

#include "containers/include/national_account.h" // lets use an auto_ptr instead.

// Forward declarations
class Subsector;
class Summary;
class Emcoef_ind;
class ILogger;
class GDP;
class Tabs;
class IInfo;
class DependencyFinder;
class Demographic;
class NationalAccount;
class MoreSectorInfo;
class SocialAccountingMatrix;
class OutputContainer;

/*! 
* \ingroup Objects
* \brief This class represents a single good that is produced, transformed, or consumed.

* All production, consumption, and transformation (other than resource extraction) is contained within the Sector class. Each Sector represents a distinct good that can either be supplied or demanded. The demand Sector derived from this class contains a few classes where changes are necessary, although most of the basic mechanisms are unchanged.

* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Sector
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
protected:
    std::string name; //!< Sector name
    std::string regionName; //!< region name
    double mBaseOutput; //!< Read in base year output.
    std::auto_ptr<IInfo> mSectorInfo; //!< Pointer to the sector's information store.
    std::vector<Subsector*> subsec; //!< subsector objects
    typedef std::vector<Subsector*>::iterator SubsectorIterator;
    typedef std::vector<Subsector*>::const_iterator CSubsectorIterator;
    std::vector<double> sectorprice; //!< Sector price in $/service
    std::vector<double> fixedOutput; //!< total amount of fixed output from Sector
    std::vector<Summary> summary; //!< summary for reporting
    std::map<std::string,int> subSectorNameMap; //!< Map of subSector name to integer position in vector.
    std::vector<bool> capLimitsPresent; //!< Flag if any capacity limits are present 
    bool anyFixedCapacity; //!< flag set to true if any fixed capacity is present in this Sector
	std::auto_ptr<MoreSectorInfo> moreSectorInfo; //! Additional sector information needed below sector

    void normalizeShareWeights( const int period );
    double getFixedShare( const unsigned int sectorNum, const int period ) const; // utility function 
    virtual void calcPrice( const int period );
    void production( const int period );
    void adjustForFixedSupply( const double marketDemand, const int period );
    void setoutput( const double demand, const int period, const GDP* gdp ); 
    void adjSharesCapLimit( const int period ); 
    void checkShareSum( const int period ) const;
    double getFixedSupply( const int period ) const; 
    bool isCapacityLimitsInSector( const int period ) const;
	double getCalOutput( const int period ) const;
	double getFixedOutput( const int period, bool printValues = false ) const; 
    virtual void printStyle( std::ostream& outStream ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual const std::string& getXMLName() const = 0;
    virtual void setMarket() = 0;
public:
    explicit Sector( std::string regionName );
    virtual ~Sector();
    const std::string& getName() const;
    virtual void XMLParse( const xercesc::DOMNode* node );
    virtual void completeInit( const IInfo* aRegionInfo, DependencyFinder* aDepFinder ) = 0;
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod ) = 0;

    virtual void calibrateSector( const int period ); 
    virtual void checkSectorCalData( const int period );
    virtual void setCalibratedSupplyInfo( const int aPeriod ) const = 0;
    void adjustForFixedOutput( const double marketDemand, const int period );
    bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;
    virtual void supply( const int aPeriod, const GDP* aGDP ) = 0;
    virtual double getOutput( const int period ) const = 0;

    bool outputsAllFixed( const int period ) const;
    bool inputsAllFixed( const int period, const std::string& goodName ) const;
    double getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals = true ) const;
    double getCalAndFixedOutputs( const int period, const std::string& goodName, const bool bothVals = true ) const;
    void setImpliedFixedInput( const int period, const std::string& goodName, const double requiredOutput );
    void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );
    double getPrice( const int period );

    virtual void calcShare( const int period, const GDP* gdp );
    virtual void calcFinalSupplyPrice( const GDP* aGdp, const int aPeriod ) = 0;
    void emission( const int period );
    void indemission( const int period, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput( const int period ) const;
    virtual double getEnergyInput( const int period );
    virtual void csvOutputFile() const;
    virtual void dbOutput() const;
    void subsec_outfile() const;
    double getTotalCarbonTaxPaid( const int period ) const;
    std::map<std::string, double> getfuelcons( const int period ) const;
    double getConsByFuel( const int period, const std::string& key) const;
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const;
    void updateSummary( const int period );
    void addToDependencyGraph( std::ostream& outStream, const int period ) const;
    void tabulateFixedDemands( const int period );

    virtual void operate( NationalAccount& nationalAccount, const Demographic* aDemographic, const int period ) = 0;	void updateMarketplace( const int period );
    virtual void finalizePeriod( const int aPeriod );
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	virtual void updateOutputContainer( OutputContainer * outputContainer, const int period ) const;

private:
    void clear();
};

#endif // _SECTOR_H_
