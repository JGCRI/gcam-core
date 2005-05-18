#ifndef _INPUT_OUTPUT_TABLE_H_
#define _INPUT_OUTPUT_TABLE_H_
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
* \file input_output_table.h
* \ingroup Objects
* \brief InputOutputTable class header file.
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
* \brief An OutputContainer that represents the IO table for a single Region.
* \details The InputOutputTable is instantiated at the Region level where it is passed to all sectors
* so that each may add its additions to the InputOutputTable. FactorSupplies and FinalDemands are also
* added to the table. The table is in currency values.
* \author Josh Lurz
*/
class Sector;
class ProductionTechnology;
class FactorSupply;
class StorageTable;
class RegionCGE;
class ProductionInput;
class DemandInput;
class Consumer;

class InputOutputTable : public OutputContainer {
public:
    InputOutputTable( const std::string& aRegionName );
    void output( std::ostream& aFile, const int period ) const;
    void updateRegionCGE( const RegionCGE* aRegion );
    void updateSector( const Sector* sector );
	void updateProductionTechnology( const ProductionTechnology* prodTech, 
		const std::string& aRegionName, const std::string& aSectorName, const int period );
    void updateProductionInput( const ProductionInput* aProdInput );
    void updateDemandInput( const DemandInput* aDemandInput );
	void updateFactorSupply( const FactorSupply* factorSupply, const int period );
    void updateConsumer( const Consumer* aConsumer, const int aPeriod );
private:
    const std::string mRegionName;
    std::auto_ptr<StorageTable> mInternalTable; //!< The internal storage structure in row-column order.
    std::string mCurrSectorName; //!< The cached name of the current sector we are adding values for.
    bool mParsingConsumer; //!< Whether a consumer is currently being parsed.
};

#endif // _INPUT_OUTPUT_TABLE_H_

