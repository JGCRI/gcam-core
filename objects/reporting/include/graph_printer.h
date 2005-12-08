#ifndef _GRAPH_PRINTER_H_
#define _GRAPH_PRINTER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file graph_printer.h
* \ingroup Objects
* \brief GraphPrinter class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include <sstream>

#include "util/base/include/default_visitor.h"

/*! 
* \ingroup Objects
* \brief A reporting class which outputs a dot graph for a specified region.
* \details A visitor which can output a dot graph for a region specified by the
*          constructor argument. The graph file must be post-processed by the dot
*          processor to create a viewable graph. The graph printer currently
*          will create nodes for each resource and sector, and links between
*          those based on prices or quantities of consumed inputs.
* \author Josh Lurz
*/
class Sector;
class Region;
class technology;

class GraphPrinter : public DefaultVisitor {
public:
    explicit GraphPrinter( const std::string& aRegionToPrint, std::ostream& aFile );
    void startVisitRegion( const Region* aRegion, const int aPeriod );
	void endVisitRegion( const Region* aRegion, const int aPeriod );
	void startVisitResource( const Resource* aResource, const int aPeriod );
    void startVisitSector( const Sector* aSector, const int aPeriod );
	void startVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod );
	void startVisitTechnology( const technology* aTechnology, const int aPeriod );
private:
    //! The file to which to write.
    std::ostream& mFile;

	//! Whether we are printing the current region.
	bool mCorrectRegion;

	//! The region for which to print graphs.
	const std::string mRegionToPrint;

	//! The current sector name
	std::string mCurrSectorName;
};

#endif // _GRAPH_PRINTER_H_
