#ifndef _SUMMARY_H_
#define _SUMMARY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file summary.h
* \ingroup CIAM
* \brief The Summary class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <string>

/*! 
* \ingroup CIAM
* \brief An object which contains variables for reporting.
* \author Sonny Kim
*/

class Summary
{
private:
	std::map<std::string, double> fuelcons;  //!< map of fuel name and amount consumed
	std::map<std::string, double> pecons;  //!< map of primary energy consumption
	std::map<std::string, double> peprod;  //!< map of primary energy production
	std::map<std::string, double> petrade;  //!< map of primary energy trade
	std::map<std::string, double> emission;  //!< map of ghg emissions
	std::map<std::string, double> emissfuel;  //!< map of ghg emissions implicit in fuel
	std::map<std::string, double> emissind;  //!< map of indirect ghg emissions
public:
	Summary(); // default construtor
	void initfuelcons( const std::string& fname, const double value );
	void initpeprod( const std::string& fname, const double value );
	void initemission( const std::string& ghgname, const double value );
	std::map<std::string, double> getfuelcons() const;
	std::map<std::string, double> getpecons() const;
	std::map<std::string, double> getpetrade() const;
	std::map<std::string, double> getemission() const;
	std::map<std::string, double> getemfuelmap() const;
	std::map<std::string, double> getemindmap() const;
	void updatefuelcons( const std::map<std::string, double>& fuelinfo);
	void updatepetrade();
	void updateemiss( const std::map<std::string, double>& ghginfo );
	void updateemfuelmap( const std::map<std::string, double>& ghginfo );
	void updateemindmap( const std::map<std::string, double>& ghginfo );
	void clearfuelcons();
	void clearpeprod();
	void clearemiss();
	void clearemfuelmap();
	void clearemindmap();
	double get_fmap_second( const std::string& name ) const;
	double get_pemap_second( const std::string& name ) const;
	double get_petrmap_second( const std::string& name ) const;
	double get_peprodmap_second( const std::string& name ) const;
	double get_emissmap_second( const std::string& name ) const;
	double get_emindmap_second( const std::string& name ) const;
    double get_emissfuelmap_second( const std::string& name ) const;
};

#endif // _SUMMARY_H_

