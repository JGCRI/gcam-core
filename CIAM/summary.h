#ifndef _SUMMARY_H_
#define _SUMMARY_H_
#pragma once

#include <map>
#include <string> 

using namespace std;

/*! 
* \ingroup CIAM
* \brief An object which contains variables for reporting.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Summary
{
private:
	map<string, double> fuelcons;  //!< map of fuel name and amount consumed
	map<string, double> pecons;  //!< map of primary energy consumption
	map<string, double> peprod;  //!< map of primary energy production
	map<string, double> petrade;  //!< map of primary energy trade
	map<string, double> emission;  //!< map of ghg emissions
	map<string, double> emissfuel;  //!< map of ghg emissions implicit in fuel
	map<string, double> emissind;  //!< map of indirect ghg emissions
public:
	Summary(); // default construtor
	void initfuelcons( const string& fname, const double value );
	void initpeprod( const string& fname, const double value );
	void initemission( const string& ghgname, const double value );
	map<string, double> getfuelcons() const;
	map<string, double> getpecons() const;
	map<string, double> getpetrade() const;
	map<string, double> getemission() const;
	map<string, double> getemfuelmap() const;
	map<string, double> getemindmap() const;
	void updatefuelcons( const map<string, double>& fuelinfo);
	void updatepetrade();
	void updateemiss( const map<string, double>& ghginfo );
	void updateemfuelmap( const map<string, double>& ghginfo );
	void updateemindmap( const map<string, double>& ghginfo );
	void clearfuelcons();
	void clearpeprod();
	void clearemiss();
	void clearemfuelmap();
	void clearemindmap();
	double get_fmap_second( const string& name ) const;
	double get_pemap_second( const string& name ) const;
	double get_petrmap_second( const string& name ) const;
	double get_peprodmap_second( const string& name ) const;
	double get_emissmap_second( const string& name ) const;
	double get_emindmap_second( const string& name ) const;
};

#endif // _SUMMARY_H_