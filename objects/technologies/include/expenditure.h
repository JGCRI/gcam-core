#ifndef _EXPENDITURE_H_
#define _EXPENDITURE_H_
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
* \file expenditure.h
* \ingroup Objects
* \brief Expenditure class header file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include <string>
#include <vector>

class Tabs;

/*! 
* \ingroup Objects
* \brief A class to track various types of expenditures by Consumers or
*        ProductionTechnologies.
* \author Pralit Patel, Sonny Kim
*/
class Expenditure
{
public:
    //! An enumeration of all possible types of expenditure.
    enum ExpenditureType {
        SOCIAL_SECURITY_TAX,
        SAVINGS,
        TAXABLE_INCOME,
        DIRECT_TAXES,
        TRANSFERS,
        DISPOSABLE_INCOME,
        CONSUMPTION,
        INCOME,
        BUDGET,
		SUBSIDY,
		INVESTMENT,
		TOTAL_IMPORTS,
		// for production sectors
		DIVIDENDS,
		RETAINED_EARNINGS,
		INDIRECT_TAXES,
		INTERMEDIATE_INPUTS,
		WAGES,
		LAND_RENTS,
		RENTALS,
		TARIFFS,
		IMPORTS,
		SALES,
		COSTS,
        END
    };

	Expenditure();
	void reset();
	void setType( const ExpenditureType aType, const double aValue );
	void addToType( const ExpenditureType aType, const double aValue );
    double getValue( const ExpenditureType aType ) const;
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	const std::string& enumToName( const ExpenditureType aType ) const;
private:
    //! Vector of expenditures indexed by type.
    std::vector<double> mExpenditures;
};

#endif // _EXPENDITURE_H_
