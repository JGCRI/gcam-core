#ifndef _GOVT_RESULTS_H_
#define _GOVT_RESULTS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
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

#include "util/base/include/default_visitor.h"


class Sector;
class ProductionTechnology;
class ProductionSector;
class RegionCGE;
class GovtConsumer;
class StorageTable;

/*! 
* \ingroup Objects
* \brief A visitor which collects the total results for the goverment final
*        demand sector.
* \details ADD HERE
* \author Josh Lurz
*/
class GovtResults : public DefaultVisitor {
public:
    GovtResults( const std::string& aRegionName, std::ostream& aFile );
    void finish() const;
    void startVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod );
    void startVisitSector( const Sector* aSector, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, const int period );
    void updateGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod );
    void updateHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer, const int aPeriod );
private:
    //! The file to which to write.
    std::ostream& mFile;
    const std::string mRegionName; //!< The name of the region this container is reporting for.
    std::auto_ptr<StorageTable> mTaxReceipts; //!< The tax receipts storage table.
    std::auto_ptr<StorageTable> mSubsidies; //!< The subsidies storage table.
    std::auto_ptr<StorageTable> mGovtExpenditures; //!< The government expenditures storage table.
    std::string mCurrSectorName; //!< The cached name of the current sector we are adding values for.
    double mGovtTransfers; //!< Total government transfers.
    bool mParsingGovt; //!< Whether we are currently in the govt consumer.
};

#endif // _GOVT_RESULTS_H_

