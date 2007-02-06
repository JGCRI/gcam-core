#ifndef _INTERM_SUBSECTOR_H_
#define _INTERM_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file interm_subsector.h
 * \ingroup Objects
 * \brief The IntermittentSubsector class header file.
 * \author Marshall Wise
 */

#include <string>
#include "sectors/include/subsector.h"
#include "sectors/include/ibackup_calculator.h"

class GDP;
class IInfo;

/*!
 * \ingroup Objects
 * \brief A Subsector that produces output from an intermittent resource.
 * \details An intermittent subsector represents the production of a good, such
 *          as electricity, from an intermittent resource, such as wind or
 *          solar. An intermittent subsector has a pair of technologies. One
 *          Technology consumes the intermittent resource and produces the
 *          majority of the output, and the other Technology produces the backup
 *          required. The backup Technology may produce a small amount of
 *          output, and emissions. The intermittent and backup technologies do
 *          not compete. The intermittent subsector has a backup calculator,
 *          which is responsible for determining the average and marginal quantity
 *          of backup capacity required. The backup calculator sets the shares
 *          of the technologies using the marginal backup requirements. These
 *          shares are used for the cost calculation, but not the output
 *          calculation. Output, and therefore emissions, is based on the
 *          average backup required.
 * \note An intermittent subsector must have two and only two Technologies, one
 *       consuming an intermittent resource and one which is the backup.
 * \note If a backup calculator is not read in, the backup requirement is
 *       assumed to be zero and this subsector will operate exactly the same as
 *       a standard Subsector with one Technology.
 * \author Marshall Wise
 */
class IntermittentSubsector: public Subsector {
public:
    IntermittentSubsector( const std::string& aRegionName,
                           const std::string& aSectorName );

    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aSectorInfo,
                               DependencyFinder* aDependencyFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo,
                           const int aPeriod );

	virtual double getPrice( const GDP* aGDP,
                             const int aPeriod ) const;
    
    virtual void setOutput( const double aVariableDemand,
                            const double aFixedOutputScaleFactor,
		                    const GDP* aGDP,
                            const int aPeriod );

	virtual const std::vector<double> calcTechShares ( const GDP* aGDP,
                                                       const int aPeriod ) const;
    
    virtual void MCoutputSupplySector( const GDP* aGDP ) const;
protected:
	//! A calculator which determines the amount of backup per unit output.
	std::auto_ptr<IBackupCalculator> mBackupCalculator;

    //! name of the electricity sector it will supply to
    std::string electricSectorName; 

    //! ordering number of technology with main Resource input (typically 0)
    unsigned int resourceTechNumber;
    
	//! Ordering number of technology with backupsector input(typically 1).
    unsigned int backupTechNumber; 

    const std::string& getXMLName() const;

	double getMarginalBackupCapacity( const int aPeriod ) const;

    double getAverageBackupCapacity( const int aPeriod ) const;

    double calcEnergyFromBackup() const;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr );

    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
};

#endif // _INTERM_SUBSECTOR_H_
