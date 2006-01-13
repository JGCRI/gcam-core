#ifndef _GHG_OUTPUT_AGGR_H_
#define _GHG_OUTPUT_AGGR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_output_aggr.h
* \ingroup Objects
* \brief The GhgOutputAggr class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include <string>
#include "emissions/include/ghg_output.h"

/*! 
* \ingroup Objects
* \brief A GHG which drives emissions from the output value based on an
*        aggregate emissions coefficient.
* \details TODO
* \author Steve Smith
*/
class GhgOutputAggr: public GhgOutput {
public:
    GhgOutputAggr();
    GhgOutputAggr* clone() const;
    static const std::string& getXMLNameStatic();
    virtual void initCalc( const IInfo* aLocalInfo );
protected:
    const std::string& getXMLName() const;
};

#endif // _GHG_OUTPUT_AGGR_H_

