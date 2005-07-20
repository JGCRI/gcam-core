#ifndef _INVESTMENT_UTILS_H_
#define _INVESTMENT_UTILS_H_
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
* \file investment_utils.h
* \ingroup Objects
* \brief The InvestmentUtils class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>

class IInvestable;

/*! 
* \ingroup Objects
* \brief A set of utility functions for calculating investment.
* \author Josh Lurz
*/
class InvestmentUtils
{
public:
    typedef std::vector<IInvestable*>::const_iterator CInvestableIterator;
    typedef std::vector<IInvestable*>::iterator InvestableIterator;
    
    static double interpolateAndSumFlows( const double aPrevInvestment, const double aCurrInvestment,
                                          const int aIntervalYears );

    static double calcBaseCapital( const std::string& aRegionName, 
                                   const double aPrevInvestment,
                                   const double aAggInvFrac,
                                   const int aPeriod );

    static double sumInvestment( const std::vector<IInvestable*>& aInvestables,
                                 const int aPeriod );

    static double normalizeShares( std::vector<double>& aShares );

    static double sumFixedInvestment( const std::vector<IInvestable*>& aInvestables,
                                       const int aPeriod );

    /*! \brief Template function which converts a vector of IInvestable subtypes
    *          to a vector of Investables.
    * \details Convert the vector of IInvestable subtypes to a vector of
    *          IInvestable objects. This is legal given that the template class
    *          passed in is a subtype of IInvestable. This will fail at
    *          compilation time if this is not true. This must be done
    *          explicitally because even if class B inherits from class A,
    *          vector<B> does not inherit from vector<A>.
    * \param aSubtypeVector A vector of subtypes of IInvestable to upcast.
    * \return A vector of IInvestable pointers corresponding to the subtype
    *         pointer vector passed in.
    */
    template <class T> 
        static std::vector<IInvestable*> convertToInvestables( const std::vector<T>& aSubtypeVector ){
            std::vector<IInvestable*> investables( aSubtypeVector.size() );
            std::copy( aSubtypeVector.begin(), aSubtypeVector.end(), investables.begin() );
            return investables;
        }
};

#endif // _INVESTMENT_UTILS_H_
