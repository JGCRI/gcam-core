#ifndef _REGION_CGE_H_
#define _REGION_CGE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


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
