/* summary.cpp									*
 * Method definition for summary class.			*
 * Summany contains variables for reporting.	*
 *												*
 * SHK  7/25/02									*/

#include "summary.h"


// summary class method definition
Summary::Summary(void) // default constructor
{
}

Summary::~Summary(void)
{
}

void Summary::initfuelcons(string fname, double value)
{
	fuelcons[fname] += value;
	fuelcons["zTotal"] += value;
}

void Summary::initpeprod(string fname, double value)
{
	peprod[fname] += value;
	peprod["zTotal"] += value;
}

void Summary::initemission(string ghgname, double value)
{
	emission[ghgname] += value;
	emission["zTotal"] += value;
}

map<string, double> Summary::getfuelcons(void)
{
	return fuelcons;
}

map<string, double> Summary::getpecons(void)
{
	return pecons;
}

map<string, double> Summary::getpetrade(void)
{
	return petrade;
}

map<string, double> Summary::getemission(void)
{
	return emission;
}

map<string, double> Summary::getemfuelmap(void)
{
	return emissfuel;
}

map<string, double> Summary::getemindmap(void)
{
	return emissind;
}

void Summary::updatefuelcons(map<string, double> fuelinfo)
{
	string str;
	typedef map<string,double>:: const_iterator CI;

	// map all primary and secondary fuel consumption
	for (CI fmap=fuelinfo.begin(); fmap!=fuelinfo.end(); ++fmap) {
		str = fmap->first;
		double test = fmap->second;
		fuelcons[fmap->first] += fmap->second;
	}

	// map primary energy consumption only
	fmap=fuelinfo.find("crude oil");
	if(fmap!=fuelinfo.end()) {
		pecons[fmap->first] += fmap->second;
		pecons["zTotal"] += fmap->second;
	}
	fmap=fuelinfo.find("natural gas");
	if(fmap!=fuelinfo.end()) {
		pecons[fmap->first] += fmap->second;
		pecons["zTotal"] += fmap->second;
	}
	fmap=fuelinfo.find("coal");
	if(fmap!=fuelinfo.end()) {
		pecons[fmap->first] += fmap->second;
		pecons["zTotal"] += fmap->second;
	}
	fmap=fuelinfo.find("uranium");
	if(fmap!=fuelinfo.end()) {
		pecons[fmap->first] += fmap->second;
		pecons["zTotal"] += fmap->second;
	}
	fmap=fuelinfo.find("renewable");
	if(fmap!=fuelinfo.end()) {
		pecons[fmap->first] += fmap->second;
		pecons["zTotal"] += fmap->second;
	}	
}

void Summary::updatepetrade(void)
{
	string str;
	typedef map<string,double>:: const_iterator CI;

	// map all primary and secondary fuel consumption
	for (CI fmap=peprod.begin(); fmap!=peprod.end(); ++fmap) {
		str = fmap->first;
		petrade[fmap->first]=peprod[fmap->first]-pecons[fmap->first];
	}
}

void Summary::updateemiss(map<string, double> ghginfo)
{
	string str;
	typedef map<string,double>:: const_iterator CI;

	// map all primary and secondary fuel consumption
	for (CI fmap=ghginfo.begin(); fmap!=ghginfo.end(); ++fmap) {
		str = fmap->first;
		double test = fmap->second;
		emission[fmap->first] += fmap->second;
	}
}

void Summary::updateemfuelmap(map<string, double> ghginfo)
{
	string str;
	typedef map<string,double>:: const_iterator CI;

	// map all primary and secondary fuel consumption
	for (CI fmap=ghginfo.begin(); fmap!=ghginfo.end(); ++fmap) {
		str = fmap->first;
		double test = fmap->second;
		emissfuel[fmap->first] += fmap->second;
	}
}

void Summary::updateemindmap(map<string, double> ghginfo)
{
	string str;
	typedef map<string,double>:: const_iterator CI;

	// map all primary and secondary fuel consumption
	for (CI fmap=ghginfo.begin(); fmap!=ghginfo.end(); ++fmap) {
		str = fmap->first;
		double test = fmap->second;
		emissind[fmap->first] += fmap->second;
	}
}

void Summary::clearfuelcons(void)
{
	fuelcons.clear();
	pecons.clear();
}

void Summary::clearpeprod(void)
{
	peprod.clear();
	petrade.clear();
}

void Summary::clearemiss(void)
{
	emission.clear();
}

void Summary::clearemfuelmap(void)
{
	emissfuel.clear();
}

void Summary::clearemindmap(void)
{
	emissind.clear();
}

double Summary::get_fmap_second(string str)
{
	return fuelcons[str];
}

double Summary::get_pemap_second(string str)
{
	return pecons[str];
}

double Summary::get_petrmap_second(string str)
{
	return petrade[str];
}

double Summary::get_peprodmap_second(string str)
{
	return peprod[str];
}

double Summary::get_emissmap_second(string str)
{
	return emission[str];
}

double Summary::get_emindmap_second(string str)
{
	return emissind[str];
}
