#ifndef _SECTOR_RESULTS_H_
#define _SECTOR_RESULTS_H_
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
* \file sector_results.h
* \ingroup Objects
* \brief SectorResults class header file.
*
*  Detailed description.
*
* \author Josh Lurz
*/

#include <string>
#include <iosfwd>
#include <memory>

#include "util/base/include/default_visitor.h"


class Sector;
class ProductionTechnology;
class ProductionSector;
class StorageTable;

/*! 
* \ingroup Objects
* \brief A visitor that collects the total results for all sectors within a
*        region.
* \details ADD HERE
* \author Josh Lurz
*/
class SectorResults : public DefaultVisitor {
public:
    SectorResults( const std::string& aRegionName, std::ostream& aFile );
    void finish() const;
    void startVisitSector( const Sector* aSector, const int aPeriod );
    void updateProductionSector( const ProductionSector* aProdSector, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, const int period );
private:
    //! The current region name.
    const std::string mCurrentRegionName;

    //! The current sector name name.
    std::string mCurrentSectorName;
    
    //! The file to which to write.
    std::ostream& mFile;

    std::auto_ptr<StorageTable> mInternalTable; //!< The internal storage structure in row-column order.
};

#endif // _SECTOR_RESULTS_H_

