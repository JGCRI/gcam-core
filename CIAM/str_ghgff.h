/* str_ghgff.h						*
 * structure for greenhouse gases	*
 * from fossil resources only.		*/


// Struct for the collection of Greenhouse gases.
// Stores emissions for each gas for region, sector, subsector
// and technology totals.
struct str_ghgff
{
	double CO2; // CO2 emissions (tgC or MTC)
	double CH4; // CH4 emissions (tgC or MTC)
	double SOX; // SOX emissions (tgC or MTC)
	double N2O; // N2O emissions (tgC or MTC)
};

