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


/*!
 * \file gcam_fusion.cpp
 * \ingroup util
 * \brief GCAMFusion class source file.
 * \author Pralit Patel
 */

#include <vector>
#include <string>

#include "util/base/include/gcam_fusion.hpp"

FilterStep* parseFilterStepStr( const std::string& aFilterStepStr ) {
    auto openBracketIter = std::find( aFilterStepStr.begin(), aFilterStepStr.end(), '[' );
    if( openBracketIter == aFilterStepStr.end() ) {
        std::cout << "Descendant filter step" << std::endl;
        // no filter just the data name
        return new FilterStep( aFilterStepStr );
    }
    else {
        std::string dataName( aFilterStepStr.begin(), openBracketIter );
        std::cout << "Data name is: >" << dataName << "<" << std::endl;
        std::string filterStr( openBracketIter + 1, std::find( openBracketIter, aFilterStepStr.end(), ']' ) );
        std::vector<std::string> filterOptions;
        boost::split( filterOptions, filterStr, boost::is_any_of( "," ) );
        std::cout << "matcher is: >" << filterOptions[ 1 ] << "< with value >" << filterOptions[ 2 ] << "< on filter >" << filterOptions[ 0 ] << "<" << std::endl;
        // [0] = filter type (name, year, index)
        // [1] = match type
        // [2:] = match type options
        AMatchesValue* matcher = 0;
        if( filterOptions[ 1 ] == "StringEquals" ) {
            matcher = new StringEquals( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "StringRegexMatches" ) {
            matcher = new StringRegexMatches( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "IntEquals" ) {
            matcher = new IntEquals( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThan" ) {
            matcher = new IntGreaterThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThanEq" ) {
            matcher = new IntGreaterThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThan" ) {
            matcher = new IntLessThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThanEq" ) {
            matcher = new IntLessThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else {
            std::cout << "Didn't match matcher" << std::endl;
        }
        
        FilterStep* filterStep = 0;
        if( filterOptions[ 0 ] == "IndexFilter" ) {
            filterStep = new FilterStep( dataName, new IndexFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "NamedFilter" ) {
            filterStep = new FilterStep( dataName, new NamedFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "YearFilter" ) {
            filterStep = new FilterStep( dataName, new YearFilter( matcher ) );
        }
        else {
            std::cout << "Didn't match filter" << std::endl;
        }
        return filterStep;
    }
}

std::vector<FilterStep*> parseFilterString( const std::string& aFilterStr ) {
    std::vector<std::string> filterStepsStr;
    boost::split( filterStepsStr, aFilterStr, boost::is_any_of( "/" ) );
    std::vector<FilterStep*> filterSteps( filterStepsStr.size() );
    for( size_t i = 0; i < filterStepsStr.size(); ++i ) {
        filterSteps[ i ] = parseFilterStepStr( filterStepsStr[ i ] );
    }
    return filterSteps;
}
