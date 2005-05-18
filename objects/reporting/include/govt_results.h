#ifndef _GOVT_RESULTS_H_
#define _GOVT_RESULTS_H_
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
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file govt_results.h
* \ingroup Objects
* \brief GovtResults class header file.
*
*  Detailed description.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <memory>

#include "reporting/include/output_container.h"

/*! 
* \ingroup Objects
* \brief An OutputContainer that represents the total results for the goverment final demand sector.
* \details ADD HERE
* \author Josh Lurz
*/
class Sector;
class ProductionTechnology;
class ProductionSector;
class RegionCGE;
class GovtConsumer;

class StorageTable;

class GovtResults : public OutputContainer {
public:
    GovtResults( const std::string& aRegionName );
    void output( std::ostream& aFile, const int period ) const;
    void updateRegionCGE( const RegionCGE* aRegionCGE );
    void updateSector( const Sector* aSector );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName, const int period );
    void updateGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod );
    void updateHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, 
const int aPeriod );
private:
    const std::string mRegionName; //!< The name of the region this container is reporting for.
    std::auto_ptr<StorageTable> mTaxReceipts; //!< The tax receipts storage table.
    std::auto_ptr<StorageTable> mSubsidies; //!< The subsidies storage table.
    std::auto_ptr<StorageTable> mGovtExpenditures; //!< The government expenditures storage table.
    std::string mCurrSectorName; //!< The cached name of the current sector we are adding values for.
    double mGovtTransfers; //!< Total government transfers.
    bool mParsingGovt; //!< Whether we are currently in the govt consumer.
};

#endif // _GOVT_RESULTS_H_

