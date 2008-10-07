#ifndef _COMPLEX_CARBON_PRINTER_H_
#define _COMPLEX_CARBON_PRINTER_H_
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
* \file complex_carbon_printer.h
* \ingroup Objects
* \brief ComplexCarbonPrinter class header file.
* \author Jim Naslund
*/

#include <stack>
#include <string>

#include "util/base/include/default_visitor.h"
#include "util/base/include/xml_helper.h"

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

class ComplexCarbonPrinter : public DefaultVisitor {
public:
    explicit ComplexCarbonPrinter( const std::string& aRegionToPrint, std::ostream& aFile );
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

    Tabs mTabs;

};

#endif // _COMPLEX_CARBON_PRINTER_H_
