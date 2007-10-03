#ifndef _COMPLEX_CARBON_DOT_PRINTER_H_
#define _COMPLEX_CARBON_DOT_PRINTER_H_
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
* \file complex_carbon_dot_printer.h
* \ingroup Objects
* \brief ComplexCarbonDotPrinter class header file.
* \author Jim Naslund
*/

#include <stack>
#include <string>

#include "util/base/include/default_visitor.h"

/*! 
* \ingroup Objects
* \brief A reporting class which outputs a dot graph of the land allocator for a specified region.
* \details A visitor which can output a dot graph of the land allocator for a region specified by
*          the constructor argument. The graph file must be post-processed by the dot processor to
*          create a viewable graph. The graph printer currently will create nodes for each node in
*          the land allocator, and links between parents and children.  Internal nodes are outputted
*          as circles, leaf nodes are outputted as boxes.
* \author Jim Naslund
*/

class ALandAllocatorItem;

class ComplexCarbonDotPrinter : public DefaultVisitor {
public:
    explicit ComplexCarbonDotPrinter( const std::string& aRegionToPrint, std::ostream& aFile );
    virtual void startVisitRegion( const Region* aRegion, const int aPeriod );
    virtual void endVisitRegion( const Region* aRegion, const int aPeriod );
    virtual void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                       const int aPeriod );
    virtual void endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                     const int aPeriod );
    virtual void startVisitCarbonBox( const CarbonBox* aCarbonBox,
                                       const int aPeriod );
    virtual void startVisitCarbonFlow( const ACarbonFlow* aCarbonFlow,
                                       const int aPeriod );


private:
    //! The file to which to write.
    std::ostream& mFile;
    
    //! Whether we are printing the current region.
    bool mCorrectRegion;
    
    //! The region for which to print graphs.
    const std::string mRegionToPrint;

    std::string mCurrentBox; 

};

#endif // _COMPLEX_CARBON_DOT_PRINTER_H_
