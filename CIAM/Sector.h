#ifndef _SECTOR_H_
#define _SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Sector.h
* \ingroup CIAM
* \brief The sector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>

// Forward declarations
class subsector;
class Summary;
class Emcoef_ind;

/*! 
* \ingroup CIAM
* \brief This class represents a single good that is produced, transformed, or consumed.

* All production, consumption, and transformation (other than resource extraction) is contained within the sector class. Each sector represents a distinct good that can either be supplied or demanded. The demand sector derived from this class contains a few classes where changes are necessary, although most of the basic mechanisms are unchanged.

* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class sector
{
protected:
    std::string name; //!< sector name
    std::string regionName; //!< region name
    std::string unit; //!< unit of final product from sector
    std::string market; //!< regional market
    int nosubsec; //!< number of subsectors in each sector
    double tax; //!< sector tax or subsidy
    bool debugChecking; //!< General toggle to turn on various checks
    std::vector<subsector*> subsec; //!< subsector objects
    std::vector<double> sectorprice; //!< sector price in $/service
    std::vector<double> price_norm; //!< sector price normalized to base year
    std::vector<double> pe_cons; //!< sectoral primary energy consumption
    std::vector<double> input; //!< sector total energy consumption
    std::vector<double> output; //!< total amount of final output from sector
    std::vector<double> fixedOutput; //!< total amount of fixed output from sector
    std::vector<double> carbonTaxPaid; //!< total sector carbon taxes paid
    std::vector<Summary> summary; //!< summary for reporting
    std::map<std::string,int> subSectorNameMap; //!< Map of subSector name to integer position in vector.
    std::vector<bool> capLimitsPresent; //!< Flag if any capacity limits are present 
    bool anyFixedCapacity; //!< flag set to true if any fixed capacity is present in this sector
    double fixedShareSavedVal; //!< debugging value
    double prevVal;
    double prevPer;

    virtual void initElementalMembers();
    void sumOutput(int per); // private function, sum taken care of automatically
    void sumInput(int per); // private function, sum taken care of automatically
    double getFixedShare( const std::string& regionName, const int sectorNum, const int per ); // utility function 
    virtual void calcPrice(int per);


public:
    sector();
    virtual ~sector();
    virtual void clear();
    std::string getName() const;
    virtual void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    virtual void XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void XMLDerivedClassParseAttr( const xercesc::DOMNode* node );
    virtual void toXML( std::ostream& out ) const;
    virtual void toOutputXML( std::ostream& out ) const;
    virtual void toXMLDerivedClass( std::ostream& out ) const;
    virtual void toDebugXML( const int period, std::ostream& out ) const;
    virtual void setMarket( const std::string& regname );
    void applycarbontax( const std::string& regionName, double tax,int per);
    void addghgtax( const std::string& ghgname, const std::string& regionName, const int per);
    virtual void calcShare( const std::string& regionName, const int per, const double gnp_cap = 1 );
    void adjSharesCapLimit( const std::string& regionName, const int per ); 
    void checkShareSum( const std::string& regionName, const int per );
    void initCalc( const std::string& regionName, const int per );
    void production( const std::string& regionName,int per);
    virtual void calibrateSector( const std::string& regionName, const int per ); 
    void setoutput(const std::string& regionName, double dmd, int per); 
    void set_ser_dmd(double dmd, int per);
    void adjustForFixedSupply( const double mrkdmd, const std::string& regionName, const int per);
    void supply( const std::string& regionName, const int per );
    int getNumberOfSubSec(void) const;
    double getOutput(int per);
    double getFixedSupply(int per) const; 
    bool sectorAllCalibrated( int per );
    bool capacityLimitsInSector( int per );
    double getCalOutput(int per) const;
    double getPrice(int per);
    void emission(int per);
    void indemission( const int per, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput(int per);
    virtual void outputfile(const std::string& regname );
    void MCoutput_subsec(const std::string& regname );
    virtual void MCoutput(const std::string& regname );
    void subsec_outfile(const std::string& regname );
    double getTotalCarbonTaxPaid(int per) const;
    std::map<std::string, double> getfuelcons(int per) const;
    double getConsByFuel(const int per, const std::string& key) const;
    void clearfuelcons(int per);
    std::map<std::string, double> getemission(int per) const;
    std::map<std::string, double> getemfuelmap(int per) const;
    void updateSummary(const int per);
    void addToDependencyGraph( std::ostream& outStream, const int period );
    void setRegionName(const std::string& regname);
};

#endif // _SECTOR_H_
