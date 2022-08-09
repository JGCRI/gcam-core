#ifndef __REMAP_DATA__
#define __REMAP_DATA__

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
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */

#include <string>
#include <vector>
#include <map>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/iparsable.h"

template<typename T>
struct ReMapDataHelper {
public:
    std::string mDataName;
    std::vector<T> mInOrderOutputNames;
    std::map<T, T> mGCAMToOutputNameMap;
    size_t getIndex(const T& aGCAMName) const;
    size_t getStrideLength() const;
    std::string getName() const;
    bool XMLParse(const xercesc::DOMNode* aNode);
};

class ReMapData : public IParsable {
public:
    ReMapData();
    ~ReMapData();
    void addColumn(std::string aDataName, std::vector<std::string> aInOrderOutputNames, std::map<std::string, std::string> aGCAMToOutputNameMap);
    void addYearColumn(std::string aDataName, std::vector<int> aInOrderOutputNames, std::map<int, int> aGCAMToOutputNameMap);
    void finalizeColumns();
    void setData(std::vector<std::string>& aColValues, const int aYearValue, const double aValue);
    double* getData();
    bool XMLParse(const xercesc::DOMNode* aNode);
    size_t getArrayLength();
private:
    bool mIsInitialized;
    bool mLandNameColumn;
    std::vector<ReMapDataHelper<std::string> > mColumns;
    ReMapDataHelper<int> mYearColumn;
    double* mData;
};

#endif // __REMAP_DATA__
