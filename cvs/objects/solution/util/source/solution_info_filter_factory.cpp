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
 * \file solution_info_filter_factory.cpp
 * \ingroup Objects
 * \brief SolutionInfoFilterFactory class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <boost/algorithm/string/trim.hpp>

#include "solution/util/include/solution_info_filter_factory.h"
#include "util/logger/include/ilogger.h"

// ISolutionInfoFilter subclasses
#include "solution/util/include/all_solution_info_filter.h"
#include "solution/util/include/solvable_solution_info_filter.h"
#include "solution/util/include/solvable_nr_solution_info_filter.h"
#include "solution/util/include/market_type_solution_info_filter.h"
#include "solution/util/include/market_name_solution_info_filter.h"
#include "solution/util/include/market_matches_solution_info_filter.h"
#include "solution/util/include/has_market_flag_solution_info_filter.h"
#include "solution/util/include/unsolved_solution_info_filter.h"
#include "solution/util/include/price_greater_than_solution_info_filter.h"
#include "solution/util/include/price_less_than_solution_info_filter.h"
#include "solution/util/include/and_solution_info_filter.h"
#include "solution/util/include/or_solution_info_filter.h"
#include "solution/util/include/not_solution_info_filter.h"

using namespace std;

/*!
 * \brief Returns whether this factory can create a filter with the given xml
 *        name.
 * \param aXMLName The name of an xml element to check.
 * \return True if this factory has a filter with the given xml name, false otherwise.
 * \note The list of known solution info filters here needs to be kept in sync with
 *       the ones found in createAndParseSolutionInfoFilter.
 */
bool SolutionInfoFilterFactory::hasSolutionInfoFilter( const string& aXMLName ) {
    return AllSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || SolvableSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || SolvableNRSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || MarketTypeSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || MarketNameSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || MarketMatchesSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || HasMarketFlagSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || UnsolvedSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || PriceGreaterThanSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || PriceLessThanSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || AndSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || OrSolutionInfoFilter::getXMLNameStatic() == aXMLName
        || NotSolutionInfoFilter::getXMLNameStatic() == aXMLName;
}

ISolutionInfoFilter* SolutionInfoFilterFactory::createOperandFilter( const string& aXMLName,
                                                                     const string& aOperandValue )
{
    ISolutionInfoFilter* ret = 0;
    bool isOperandWithNoValue = false;
    if( aXMLName == AllSolutionInfoFilter::getXMLNameStatic() ) {
        isOperandWithNoValue = true;
        ret = new AllSolutionInfoFilter();
    }
    else if( aXMLName == SolvableSolutionInfoFilter::getXMLNameStatic() ) {
        isOperandWithNoValue = true;
        ret = new SolvableSolutionInfoFilter();
    }
    else if( aXMLName == SolvableNRSolutionInfoFilter::getXMLNameStatic() ) {
        isOperandWithNoValue = true;
        ret = new SolvableNRSolutionInfoFilter();
    }
    else if( aXMLName == MarketTypeSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new MarketTypeSolutionInfoFilter(aOperandValue);
    }
    else if( aXMLName == MarketNameSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new MarketNameSolutionInfoFilter(aOperandValue);
    }
    else if( aXMLName == MarketMatchesSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new MarketMatchesSolutionInfoFilter(aOperandValue);
    }
    else if( aXMLName == HasMarketFlagSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new HasMarketFlagSolutionInfoFilter(aOperandValue);
    }
    else if( aXMLName == UnsolvedSolutionInfoFilter::getXMLNameStatic() ) {
        isOperandWithNoValue = true;
        ret = new UnsolvedSolutionInfoFilter();
    }
    else if( aXMLName == PriceGreaterThanSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new PriceGreaterThanSolutionInfoFilter(aOperandValue);
    }
    else if( aXMLName == PriceLessThanSolutionInfoFilter::getXMLNameStatic() ) {
        ret = new PriceLessThanSolutionInfoFilter(aOperandValue);
    }
    
    if(isOperandWithNoValue && !aOperandValue.empty()) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Ignoring operand value: " << aOperandValue << " as " << aXMLName << " does not take any." << endl;
    }
    else if(!isOperandWithNoValue && aOperandValue.empty()) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << aXMLName << " was expecting a value but none provided." << endl;
    }
    
    // error checking for unknown operand will be handled elsewhere
    return ret;
}

/*!
 * \brief Creates a solution info filter by parsing a filter string.
 * \details The syntax for a filter string is very simple, we allow the and, or, and not
 *          operators and the operands would be the other known ISolutionInfoFilter to this
 *          factory.  We are assuming that and/or are binary operators for simplicity and
 *          along the same lines are requiring that the operand to the not be wrapped in
 *          parentheses.  Parentheses are allowed to structure grouping and extra whitespace
 *          is ignored.  To process the filter string we recursively process the given filter string
 *          to ensure we properly nest operators and parenthesis.  The following is an example of a
 *          filter string:
 *          !(solvable && (unsolved || solvable-nr)) || !(market-name="CO2") || market-type="Trial"
 * \param aFilterString A filter string using the syntax described above.
 * \return The newly created and parsed filter or null if given an invalid syntax string.
 */
ISolutionInfoFilter* SolutionInfoFilterFactory::createSolutionInfoFilterFromString( const string& aFilterString ) {
    // Have the buildSolutionInfoFilterString recursively parse the string.
    ISolutionInfoFilter* parsedSyntaxTree = buildSolutionInfoFilterString( aFilterString );

    // A null value indicates some sort of failure to parse, warn the user
    if( !parsedSyntaxTree ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Could not parse filter string: " << aFilterString << endl;
    }
    return parsedSyntaxTree;
}


/*!
 * \brief Build a SolutionInfoFilter representing the given filter string.
 * \details Recursively process the given filter string so that we can get proper syntax nesting when we have
 *          multiple operators or parenthesis to contend with.
 * \param aFilterString The filter string to parse.
 * \return An ISolutionInfoFilter tree which replicates the logic in the given string and can be used
 *         to create the filter objects.
 * \see SolutionInfoFilterFactory::createSolutionInfoFilterFromString
 */
ISolutionInfoFilter* SolutionInfoFilterFactory::buildSolutionInfoFilterString( const string& aFilterString ) {
    // Create constants for the operators, also for the filter string we assume users
    // will not include the -solution-info-filter which is the xml name style.
    const string notOperatorStr = "!";
    const string andOperatorStr = "&&";
    const string orOperatorStr = "||";
    const string xmlNameExtension = "-solution-info-filter";
    
    // first get rid of leading and trailing whitespace
    string currFilter( aFilterString );
    boost::algorithm::trim( currFilter );
    
    // We should check if the current filter is now empty which could happen
    // if for instance we had something like "solvable || " which may seem
    // like an error but we could actually process that just fine anyway.
    if( currFilter.empty() ) {
        return 0;
    }
    
    // We may need to offset our operator search to account for parentheses.
    string::const_iterator offset = currFilter.begin();
    // Check for parentheses or the not operator which is assumed to use parentheses
    // like: !( .. ) and to make things easier we will not allow spaces between the
    // not operator and open parenthesis.
    if( *offset == '(' || string( offset, offset + notOperatorStr.size() ) == notOperatorStr ) {
        // Easy way to see if we are dealing with a not is to check if the first char was
        // not an open parenthesis.
        bool isNot = !( *offset == '(' );
        string::const_iterator openParen = isNot ? offset + notOperatorStr.size() : offset;
        offset = findCloseParentheses( currFilter, openParen ) + 1;
        
        // If the parentheses encompass the entire current filter we should process it now.
        if( offset == currFilter.end() ) {
            // Before we recursively process the sub filter string we must remove the
            // parentheses.
            string subFilter( openParen + 1, offset - 1 );
            ISolutionInfoFilter* childFilter = buildSolutionInfoFilterString( subFilter );
            
            // If there was a not operator then we must wrap the parentheses with a not
            // filter otherwise we are done.
            if( isNot ) {
                // Wrap the results with a not solution info filter and return that,
                // if we got back null warn the user and return null as well.
                if( childFilter ) {
                    childFilter = new NotSolutionInfoFilter( childFilter );
                }
                else {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << "Did not create " << NotSolutionInfoFilter::getXMLNameStatic()
                            << " since the returned operand was null." << endl;
                }
            }
            return childFilter;
        }
    }
    
    // Now that we have accounted for parentheses and not operators we need to find
    // the next operator.
    // Since the we are searching for operator strings we much rely on string::find
    // which means we will have to convert back and forth from indexes and iterators.
    int tempIndex = currFilter.find( andOperatorStr, offset - currFilter.begin() );
    string::const_iterator andOpPos = tempIndex != string::npos ? currFilter.begin() + tempIndex : currFilter.end();
    tempIndex = currFilter.find( orOperatorStr, offset - currFilter.begin() );
    string::const_iterator orOpPos = tempIndex != string::npos ? currFilter.begin() + tempIndex : currFilter.end();
    
    // TODO: Not sure why the compiler thinks I need these instead of using
    //       begin and end directly.
    string::const_iterator begin = currFilter.begin();
    string::const_iterator end = currFilter.end();
    
    // When we set the operator we will use these.
    string currOpName;
    string::const_iterator opPos;
    if( andOpPos != currFilter.end() && andOpPos < orOpPos ) {
        currOpName = andOperatorStr;
        opPos = andOpPos;
    }
    else if( orOpPos != currFilter.end() ) {
        currOpName = orOperatorStr;
        opPos = orOpPos;
    }
    else {
        // This should be a non-operator solution info filter.
        // Check to see if filter has a value associated with it, which is specified
        // by an '='.  If so parse it out.
        opPos = find( currFilter.begin(), currFilter.end(), '=' );
        string filterValue;
        if( opPos != currFilter.end() ) {
            currOpName = string( begin, opPos );
            // remove the "" around the value as well
            filterValue = string( opPos + 2, end - 1);
            
            // get rid of extra whitespaces from the value, note that
            // if there was extra whitespace then we might not have gotten
            // rid of the "" so remove them again just in case
            replace( filterValue.begin(), filterValue.end(), '"', ' ');
            boost::algorithm::trim( filterValue );
        }
        else {
            currOpName = currFilter;
        }
        
        // get rid of any extra whitespace
        boost::algorithm::trim( currOpName );
        
        // If we are left with a filter we don't know about that is a problem so
        // warn the user.  Add the xml name extension so that the filter names will
        // match those from getXMLNameStatic().
        if( !hasSolutionInfoFilter( currOpName + xmlNameExtension ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Could not create unknown ISolutionInfoFilter: " << currOpName
                    << " while parsing filter string." << endl;
            return 0;
        }
        
        // Create the solution info filter operand with the correct xml name.
        // Warnings will be generated if there is some mismatch between operands
        // expecting/not expecting a value where it was/wasn't provided.
        ISolutionInfoFilter* filter = createOperandFilter( currOpName + xmlNameExtension, filterValue );
            
        return filter;
    }
    
    // For simplicity's sake I am going to treat and/or as binary operations here
    // even though they are implemented to be able to do the operation between many.
    // Process the left and right hand side of the binary operator and add them
    // as children to the xml command.
    vector<ISolutionInfoFilter*> operands(2);
    ISolutionInfoFilter* childFilter = buildSolutionInfoFilterString( string( begin, opPos ) );
    // Only add the operands if it they are not null.
    // TODO: I should I check if both the lhs and rhs were null and warn the user?
    //       currently both operators would just return true if they had no contained
    //       filters.
    if( childFilter ) {
        operands[0] = childFilter;
    }
    childFilter = buildSolutionInfoFilterString( string( opPos + currOpName.size(), end ) );
    if( childFilter ) {
        operands[1] = childFilter;
    }
    ISolutionInfoFilter* opFilter = currOpName == andOperatorStr ? static_cast<ISolutionInfoFilter*>(new AndSolutionInfoFilter(operands))
                                                                 : static_cast<ISolutionInfoFilter*>(new OrSolutionInfoFilter(operands));
    
    return opFilter;
}

/*!
 * \brief Find the close parenthesis to the open parenthesis at the given iterator in the given
 *        filter string.
 * \details We will rely on the find method to look for the closing parenthesis, if another
 *          open parenthesis is found we will need to recursively search to ensure we get the
 *          proper close.  If no close parenthesis was found aFilterString.end() will be
 *          returned.
 * \param aFilterString The filter string in which to search.
 * \param aOpenParen An iterator into aFilterString which is assumed to be pointing to
 *                   an open parenthesis for which a matched close will be found.
 * \return An iterator pointing to the proper close parenthesis or the end of the string
 *         if it could not be found.
 */
string::const_iterator SolutionInfoFilterFactory::findCloseParentheses( const string& aFilterString,
                                                                        const string::const_iterator& aOpenParen )
{
    /*!
     * \pre We assumed that aOpenParen will be pointing to an open parenthesis.
     */
    assert( *aOpenParen == '(' );
    
    string::const_iterator closeParen = find( aOpenParen + 1, aFilterString.end(), ')' );
    string::const_iterator nextOpenParen = find( aOpenParen + 1, aFilterString.end(), '(' );
    
    // check if there was another open parenthesis before the first close that we found
    if( nextOpenParen != aFilterString.end() && nextOpenParen < closeParen ) {
        // there are nested parentheses so we will have to recursively match parentheses
        closeParen = find( findCloseParentheses( aFilterString, nextOpenParen ) + 1,
                           aFilterString.end(), ')' );
    }
    
    return closeParen;
}
