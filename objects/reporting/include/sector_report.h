#ifndef _SECTOR_REPORT_H_
#define _SECTOR_REPORT_H_
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
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file sector_report.h
* \ingroup Objects
* \brief SectorReport class header file.
*
*  Detailed description.
*
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <memory>

#include "reporting/include/output_container.h"

class StorageTable;
class Sector;
class ProductionTechnology;

/*! 
* \ingroup Objects
* \brief CHANGE
* \details CHANGE
*
* \note CHANGE
* \author Sonny Kim
*/

class SectorReport : public OutputContainer {
public:
	SectorReport();
    void output( std::ostream& aFile, const int period ) const;
	void updateSector( const Sector* sector );
	void updateProductionTechnology( const ProductionTechnology* prodTechnology, 
		const std::string& aRegionName, const std::string& aSectorName, const int period );
private:
    std::auto_ptr<StorageTable> mTable; //!< Internal storage table for the SectorReport.
	std::string mSectorName; //!< sector name
	static const std::string getYearString( const int aYear );
};

#endif // _SECTOR_REPORT_H_

