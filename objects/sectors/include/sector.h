#ifndef _SECTOR_H_
#define _SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file sector.h
* \ingroup CIAM
* \brief The Sector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include <algorithm>

// Forward declarations
class Subsector;
class Summary;
class Emcoef_ind;
class Region;
class Logger;
/*! 
* \ingroup CIAM
* \brief This class represents a single good that is produced, transformed, or consumed.

* All production, consumption, and transformation (other than resource extraction) is contained within the Sector class. Each Sector represents a distinct good that can either be supplied or demanded. The demand Sector derived from this class contains a few classes where changes are necessary, although most of the basic mechanisms are unchanged.

* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Sector
{
protected:
    std::string name; //!< Sector name
    std::string regionName; //!< region name
    std::string unit; //!< unit of final product from Sector
    std::string market; //!< regional market
    int nosubsec; //!< number of subsectors in each Sector
    double tax; //!< Sector tax or subsidy
    bool debugChecking; //!< General toggle to turn on various checks
    std::vector<Subsector*> subsec; //!< subsector objects
    std::vector<double> sectorprice; //!< Sector price in $/service
    std::vector<double> price_norm; //!< Sector price normalized to base year
    std::vector<double> pe_cons; //!< sectoral primary energy consumption
    std::vector<double> input; //!< Sector total energy consumption
    std::vector<double> output; //!< total amount of final output from sector
    std::vector<double> fixedOutput; //!< total amount of fixed output from Sector
    std::vector<double> carbonTaxPaid; //!< total Sector carbon taxes paid
    std::vector<Summary> summary; //!< summary for reporting
    std::map<std::string,int> subSectorNameMap; //!< Map of subSector name to integer position in vector.
    std::vector<bool> capLimitsPresent; //!< Flag if any capacity limits are present 
    std::vector<std::string> simulList; //!< List of all sectors with simuls to this one. 
    std::vector<std::string> dependsList; //!< List of all dependencies of this Sector. 
    bool anyFixedCapacity; //!< flag set to true if any fixed capacity is present in this Sector
    double fixedShareSavedVal; //!< debugging value
    double prevVal;
    double prevPer;

    virtual void initElementalMembers();
    void sumOutput( const int period ); // private function, sum taken care of automatically
    void sumInput( const int period ); // private function, sum taken care of automatically
    double getFixedShare( const int sectorNum, const int period ) const; // utility function 
    virtual void calcPrice( const int period );
    virtual void printStyle( std::ostream& outStream ) const;

public:
    Sector( std::string regionName );
    virtual ~Sector();
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
    virtual void setMarket();
    void applycarbontax( double tax, const int period );
    void addGhgTax( const std::string& ghgname, const int period );
    virtual void calcShare( const int period, const double gnp_cap = 1 );
    void adjSharesCapLimit( const int period ); 
    void checkShareSum( const int period ) const;
    void initCalc( const int period );
    void production( const int period );
    virtual void calibrateSector( const int period ); 
    void setoutput( const double demand, const int period ); 
    void setServiceDemand( const double demand, const int period );
    void adjustForFixedSupply( const double marketDemand, const int period );
    void supply( const int period );
    double getOutput( const int period );
    double getFixedSupply( const int period ) const; 
    bool isAllCalibrated( const int period ) const;
    bool isCapacityLimitsInSector( const int period ) const;
    double getCalOutput( const int period ) const;
    double getPrice( const int period );
    void emission( const int period );
    void indemission( const int period, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput( const int period );
    virtual void outputfile() const;
    void MCoutput_subsec() const;
    virtual void MCoutput() const;
    void subsec_outfile() const;
    double getTotalCarbonTaxPaid( const int period ) const;
    std::map<std::string, double> getfuelcons( const int period ) const;
    double getConsByFuel( const int period, const std::string& key) const;
    void clearfuelcons( const int period );
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const;
    void updateSummary( const int period );
    void addToDependencyGraph( std::ostream& outStream, const int period ) const;
    void addSimul( const std::string sectorName );
    void setupForSort( const Region* parentRegion );
    std::vector<std::string> getInputDependencies( const Region* parentRegion ) const;
    const std::vector<std::string> getDependsList() const;
    void printSectorDependencies( Logger* logger ) const;
    
    /*!
    * \brief Binary function used to order Sector* pointers by input dependency. 
    * \author Josh Lurz
    *
    * The DependencyOrdering struct is used by the region class in the stl sort to compare
    * two Sector pointers and order them by dependency. It is a strict weak ordering.
    * The algorithm checks if the rhs  Sector depends on the lhs Sector with a non-simul. If this is true, it returns true as the 
    * lhs is needed before the rhs, but when a simul market exists, the ordering between two sectors is trivial.
    * Otherwise, the operator returns false. This is done so that unrelated sectors are equivalent.
    */   
    struct DependencyOrdering : public std::binary_function<Sector*, Sector*, bool>
    {
        //! \brief The () operator, which is used for sorting two Sector pointers. 

        bool operator()( const Sector* lhs, const Sector* rhs ) const {
            // First cache a copy of the dependsList
            std::vector<std::string> rhsDependsList = rhs->getDependsList();
            
            // Check if the left Sector depends on the right Sector.
            if ( std::binary_search( rhsDependsList.begin(), rhsDependsList.end(), lhs->getName() ) ) {
                return true;
            }
            else {
                return false;
            }
        }
    };
};

#endif // _SECTOR_H_
