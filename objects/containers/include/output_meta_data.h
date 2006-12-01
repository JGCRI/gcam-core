#ifndef _OUTPUT_META_DATA_H_
#define _OUTPUT_META_DATA_H_
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
* \file output_meta_data.h
* \ingroup Objects
* \brief OutputMetaData header file.
* \author Josh Lurz
*/
#include <list>
#include <string>

#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

/*! 
* \ingroup Objects
* \brief A container of read-in metadata used for outputting information and
*        passed to the dataviewer.
* \author Josh Lurz
*/

class OutputMetaData: public IVisitable, public IRoundTrippable
{
    friend class IVisitor;
public:
    OutputMetaData();
    static const std::string& getXMLNameStatic();
    void XMLParse( const xercesc::DOMNode* aNode );
    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    const std::list<std::string>& getPrimaryFuelList() const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
private:
    //! List of names of primary fuels.     
    std::list<std::string> mPrimaryFuels;

    //! List of variables which are summable.
    std::list<std::string> mSummableVariables;

    //! List of variables which have year attribute.
    std::list<std::string> mHasYearVariables;

    //! Scenario summary.
    std::string mScenarioSummary;
};

#endif // _OUTPUT_META_DATA_H_

