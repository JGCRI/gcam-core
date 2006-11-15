#ifndef _SUMMARY_H_
#define _SUMMARY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file summary.h
* \ingroup Objects
* \brief The Summary class header file.
* \author Sonny Kim
*/

#include <map>
#include <string>
#include <list>

/*! 
* \ingroup Objects
* \brief An object which contains variables for reporting.
* \author Sonny Kim
*/

class Summary
{
private:
    typedef std::map<std::string, double> SummaryItem;
    typedef SummaryItem::const_iterator CSummaryIterator;
    typedef SummaryItem::iterator SummaryIterator;
	SummaryItem fuelcons;  //!< map of fuel name and amount consumed
	SummaryItem pecons;  //!< map of primary energy consumption
	SummaryItem peprod;  //!< map of primary energy production
	SummaryItem petrade;  //!< map of primary energy trade
	SummaryItem emission;  //!< map of ghg emissions
	SummaryItem emissfuel;  //!< map of ghg emissions implicit in fuel
	SummaryItem sequesteredAmount;  //!< map of sequestered amount of emissions
public:
	Summary(); // default construtor
	void initfuelcons( const std::string& fname, const double value );
	void initpeprod( const std::string& fname, const double value );
	const SummaryItem& getfuelcons() const;
	const SummaryItem& getpecons() const;
	const SummaryItem& getpetrade() const;
	const SummaryItem& getemission() const;
	const SummaryItem& getemfuelmap() const;
	const SummaryItem& getSequesteredAmountMap() const;
	void updatefuelcons( const std::list<std::string>& aPrimaryFuelList, const SummaryItem& fuelinfo);
	void updatepetrade();
	void updateemiss( const SummaryItem& ghginfo );
	void updateemfuelmap( const SummaryItem& ghginfo );
	void updateSequesteredAmountMap( const SummaryItem& ghginfo );
    void clearfuelcons();
	void clearpeprod();
	void clearemiss();
	void clearemfuelmap();
    void clearSequesteredAmountMap();
	double get_fmap_second( const std::string& name ) const;
	double get_pemap_second( const std::string& name ) const;
	double get_petrmap_second( const std::string& name ) const;
	double get_peprodmap_second( const std::string& name ) const;
	double get_emissmap_second( const std::string& name ) const;
    double getSequesteredAmount( const std::string& name ) const;
    double get_emissfuelmap_second( const std::string& name ) const;
};

#endif // _SUMMARY_H_

