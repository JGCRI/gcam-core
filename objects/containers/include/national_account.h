#ifndef _NATIONAL_ACCOUNT_H_
#define _NATIONAL_ACCOUNT_H_
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
* \file national_account.h
* \ingroup Objects
* \brief The National Account class header file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <string>
#include <vector>

class OutputContainer;
class Tabs;
/*! 
* \ingroup Objects
* \brief CHANGE
* \details CHANGE
*
* \note CHANGE
* \author Pralit Patel, Sonny Kim
* \todo Lots of documentation!
*/

class NationalAccount
{
    friend OutputContainer;
public:
    enum AccountType {
        GDP,
        RETAINED_EARNINGS,
        SUBSIDY,
        CORPORATE_PROFITS,
        CORPORATE_RETAINED_EARNINGS,
        CORPORATE_INCOME_TAXES,
        CORPORATE_INCOME_TAX_RATE,
        PERSONAL_INCOME_TAXES,
        INVESTMENT_TAX_CREDIT,
        DIVIDENDS,
        LABOR_WAGES,
        LAND_RENTS,
        TRANSFERS,
        SOCIAL_SECURITY_TAX,
        INDIRECT_BUSINESS_TAX,
		GNP,
		GNP_VA,
		CONSUMPTION,
		GOVERNMENT,
		INVESTMENT,
		NET_EXPORT,
		EXCHANGE_RATE,
		ANNUAL_INVESTMENT,
        // Insert new values before END marker.
        END
    };
	NationalAccount();
	static const std::string& getXMLNameStatic();
	void XMLParse( const xercesc::DOMNode* node );
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	void reset();
    void addToAccount( const AccountType aType, const double aValue );
    void setAccount( const AccountType aType, const double aValue );
    double getAccountValue( const AccountType aType ) const;
	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	void updateOutputContainer( OutputContainer* outputContainer, const int aPeriod ) const;
private:
    const std::string& enumToName( const AccountType aType ) const;
    //! Vector to hold national account values
    std::vector<double> mAccounts;
};

#endif // _NATIONAL_ACCOUNT_H_

