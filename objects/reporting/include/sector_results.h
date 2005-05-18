#ifndef _SECTOR_RESULTS_H_
#define _SECTOR_RESULTS_H_
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
* \file sector_results.h
* \ingroup Objects
* \brief SectorResults class header file.
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
* \brief An OutputContainer that represents the total results for all sectors within a region.
* \details ADD HERE
* \author Josh Lurz
*/
class Sector;
class ProductionTechnology;
class ProductionSector;
class StorageTable;

class SectorResults : public OutputContainer {
public:
    SectorResults( const std::string& aRegionName );
    void output( std::ostream& aFile, const int period ) const;
    void updateSector( const Sector* aSector );
    void updateProductionSector( const ProductionSector* aProdSector, const int aPeriod );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName, const int period );
private:
    const std::string mRegionName;
    std::auto_ptr<StorageTable> mInternalTable; //!< The internal storage structure in row-column order.
    std::string mCurrSectorName; //!< The cached name of the current sector we are adding values for.
};

#endif // _SECTOR_RESULTS_H_

