#ifndef _STR_GHGSS_H_
#define _STR_GHGSS_H_
#pragma once

/*! 
* \file str_ghgss.h
* \ingroup CIAM
* \brief The str_ghgss struct header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

/*! 
* \ingroup CIAM
* \brief Struct for the collection of Greenhouse gases.
*
* Stores emissions for each gas for region, sector, subsector
* and technology totals.
* \author Sonny Kim
*/

struct str_ghgss
{
	double CO2; //!< total CO2 emissions by fuel (tgC or MTC)
	double CO2oil; //!< CO2 emissions from crude oil (tgC or MTC)
	double CO2gas; //!< CO2 emissions from natural gas (tgC or MTC)
	double CO2coal; //!< CO2 emissions from coal (tgC or MTC)
	double CO2s; //!< CO2 emissions by sector (tgC or MTC)
	double CH4; //!< CH4 emissions (tgC or MTC)
	double SOX; //!< SOX emissions (tgC or MTC)
	double N2O; //!< N2O emissions (tgC or MTC)
	double CO2ind; //!< indirect CO2 emissions (tgC or MTC)
	double CO2fuel; //!< equivalent CO2 emissions from fuel input (tgC or MTC)
	double CO2ag; //!< CO2 from agriculture for magicc/climat
	double SOXreg1; //!< SOX emissions region 1 for magicc (tgC or MTC)
	double SOXreg2; //!< SOX emissions region 2 for magicc (tgC or MTC)
	double SOXreg3; //!< SOX emissions region 3 for magicc (tgC or MTC)
	double CF4; //!< for magicc/climat 
	double C2F6; //!< for magicc/climat 
	double HFC125; //!< for magicc/climat 
	double HFC134a; //!< for magicc/climat 
	double HFC143a; //!< for magicc/climat 
	double HFC227ea; //!< for magicc/climat 
	double HFC245ca; //!< for magicc/climat
	double SF6; //!< for magicc/climat
};

#endif // _STR_GHGSS_H_