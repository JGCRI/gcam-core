package ModelInterface.DMsource;

import org.jdom.Element;
import org.jdom.Document;
import org.jdom.output.DOMOutputter;

import java.util.logging.Logger;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Iterator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.File;
import java.io.PrintStream;

import ModelInterface.ModelGUI2.xmldb.XMLDB;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.tables.ComboTableModel;
import ModelInterface.ModelGUI2.tables.MultiTableModel;
import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.ScenarioListItem;

/**
 * This class contains static methods for downscaling emissions. This class uses the methodology
 * described in the paper avaiable online at www.sciencedirect.com: TODO: proper style
 * Downscaling drivers of global environmental change.  Enabling user of 
 * 	global SRES scenarios at the national and grid levels
 * 	Detlef P. van Vuuren, Paul L Lucas, Henk Kilderink; 24 April 2006
 *
 * Note that this class is separated from ComponentManipulator so that dependencies on other
 * parts of the ModelInterface and be easily removed if necessary.
 * @author Pralit Patel
 */
public class EmissionsDownscaler {
	/**
	 * Start point to do downscaling gathers all the data and checks for consisteny,
	 * then starts the calculations.
	 * @param command The xml command that tells us where to get our data.
	 * @param driver The ManipulationDriver that is running this command. 
	 */
	public static boolean doDownscaling(Element command, ManipulationDriver driver) {
		// log class to use for all logging output
		final Logger log = Logger.getLogger("DataManipulation"); 
		Element currInfo;
		// TODO: get base year
		final int BASE_YEAR = 2005;
		// we define a max allowed convergence year if the user reads in one greater
		// than this we will assume no convergence in emissions and the base year
		// pattern will be used
		final int MAX_CONV_YEAR = 5000;
		currInfo = command.getChild("convergent-year");
		final int CONV_YEAR = Integer.valueOf(currInfo.getAttributeValue("value"));
		if(CONV_YEAR > MAX_CONV_YEAR) {
			log.warning("Convergence year "+CONV_YEAR+" is greater than the max: "
					+MAX_CONV_YEAR+" we will assume no emissions convergence.");
		}
		final List<Region> minicamRegions = ((superRegion)driver.regionList.get("World-MiniCAM")).data;
		currInfo = command.getChild("show-query-table");
		final boolean showQueryTable = currInfo == null;

		// Initialize and get data from the xml database that has the model's
		// output
		BaseTableModel populationTable = null;
		BaseTableModel gdpTable = null;
		BaseTableModel emissTable = null;
		try {
			// set up all additional conf values
			currInfo = command.getChild("xmldb-location");
			final String dbLocation = currInfo.getAttributeValue("value");
			currInfo = command.getChild("scenario");
			final String scnName = currInfo.getAttributeValue("name");
			final String scnDate = currInfo.getAttributeValue("date");
			final Object[] scenarioList = new Object[1];
			scenarioList[0] = new ScenarioListItem(null, scnName, scnDate);
			// hard coded as all regions
			final Object[] regionList = new Object[minicamRegions.size()];
			for(int i = 0; i < minicamRegions.size(); ++i) {
				regionList[i] = minicamRegions.get(i).name.equals("China/CPA") ?
					"China" : minicamRegions.get(i).name;
			}

			XMLDB.openDatabase(dbLocation, null);
			populationTable = getModelData(command.getChild("populationQuery"), 
					scenarioList, regionList);
			gdpTable = getModelData(command.getChild("gdpQuery"), 
					scenarioList, regionList);
			emissTable = getModelData(command.getChild("emissionsQuery"),
					scenarioList, regionList);
			if(showQueryTable) {
				javax.swing.JDialog jd = new javax.swing.JDialog();
				javax.swing.JTable tbl = new javax.swing.JTable(emissTable);
				javax.swing.JScrollPane sp = new javax.swing.JScrollPane(tbl);
				jd.setContentPane(sp);
				jd.pack();
				jd.setVisible(true);
			}
		} catch(Exception e) {
			log.severe("Error getting data from database: "+e);
			e.printStackTrace();
			return false;
		} finally {
			XMLDB.closeDatabase();
		}
		final int popBaseYearIndex = populationTable.findColumn(String.valueOf(BASE_YEAR));
		if(popBaseYearIndex == -1) {
			log.severe("Could not find base year "+BASE_YEAR+" in the table column names");
			return false;
		}
		// all model data must have be retrieved by this point

		// initialize the country population data.
		// the population by country stored as a Map of Time -> Region -> Data
		final Map<Integer, Map<String, Double>> POPc = new HashMap<Integer, Map<String, Double>>();
		final Map<Integer, Map<String, Double>> Ar = new HashMap<Integer, Map<String, Double>>();

		currInfo = command.getChild("population-file");
		final String popFileName = currInfo.getAttributeValue("value");
		BufferedReader buff = null;
		try {
			buff = new BufferedReader(new FileReader(popFileName));
			// first line is the country names
			String currLine = buff.readLine();
			final String[] regionNames = currLine.split(",");
			// iterate over each row which is by time
			while((currLine = buff.readLine()) != null) {
				final String[] currLineSplit = currLine.split(",");
				final Map<String, Double> currCountryMap = 
					new HashMap<String, Double>(currLineSplit.length-1);
				final Map<String, Double> currRegionMap = 
					new HashMap<String, Double>(minicamRegions.size());
				// initialize region population sums to zero
				for(Iterator<Region> it = minicamRegions.iterator(); it.hasNext(); ) {
					currRegionMap.put(it.next().name, 0.0);
				}
				// set the time then iteratate over the countries
				POPc.put(Integer.valueOf(currLineSplit[0]), currCountryMap);
				Ar.put(Integer.valueOf(currLineSplit[0]), currRegionMap);
				for(int i = 1; i < currLineSplit.length; ++i) {
					Double currValue = Double.valueOf(currLineSplit[i]);
					// set country popuation (Ac in eq(1)) for current time
					currCountryMap.put(regionNames[i], currValue);

					// calculate region population sums (Ar in eq(1)) for current time
					Region minicamSuperRegion = getSuperRegion(minicamRegions, regionNames[i]);
					if(minicamSuperRegion != null) {
						Double regionValue = currRegionMap.get(minicamSuperRegion.name);
						regionValue += currValue;
						currRegionMap.put(minicamSuperRegion.name, regionValue);
					} else {
						log.warning("Could not find super region for "+regionNames[i]);
					}
				}
			}
		} catch(IOException ioe) {
			log.severe("Couldn't read population file: "+ioe);
			ioe.printStackTrace();
			return false;
		} finally {
			if(buff != null) {
				// just close ignore any errors
				try {buff.close();} catch(IOException ioe) {}
			}
		}
		int popIndex = popBaseYearIndex;
		final int popRegionIndex = populationTable.findColumn("region");
		if(popRegionIndex == -1) {
			log.severe("Could not find region column of table");
			return false;
		}
		final List<Integer> calcYears = new ArrayList<Integer>();
		while(!populationTable.getColumnName(popIndex).equals("Units")) {
			// assert the other tables are not to Units yet
			int currYear = Integer.valueOf(populationTable.getColumnName(popIndex));
			calcYears.add(currYear);
			Map<String, Double> currCountryMap = 
				POPc.get(currYear);
			Map<String, Double> currRegionMap = 
				Ar.get(currYear);
			for(int j = 0; j < populationTable.getRowCount(); ++j) {
				final String currMinicamRegionName = checkRegion((String)populationTable.getValueAt(j, popRegionIndex));
				Region currRegion = null;
				for(Iterator<Region> it = minicamRegions.iterator(); it.hasNext(); ) {
					Region tempRegion = it.next();
					if(tempRegion.name.equals(currMinicamRegionName)) {
						currRegion = tempRegion;
						break;
					}
				}
				assert(currRegion != null);
				for(Iterator<Map.Entry<String, Double>> it = currCountryMap.entrySet().iterator(); it.hasNext(); ) {
					Map.Entry<String, Double> currCountry = it.next();
					if(currRegion.containsRegion(currCountry.getKey())) {
						/**
						 * Evaluating eq(1): POPc = POPr(Ac/Ar)
						 * With:
						 * POPr = populationTable[j][popIndex]
						 * Ac = currCountryMap[currCountry.getKey()]
						 * Ar = currRegionMap[currMinicamRegionName]
						 * POPC = currCountryMap[currCountry.getKey()] (will be overwritten)
						 */ 
						final double POPc_temp = (Double)populationTable.getValueAt(j, popIndex) *
							(currCountry.getValue() / currRegionMap.get(currMinicamRegionName));
						currCountry.setValue(POPc_temp);
					}
				}
			}
			++popIndex;
		}

		currInfo = command.getChild("emissions-conversion-factor");
		final double emissConvFactor = Double.valueOf(currInfo.getAttributeValue("value"));

		// initialize region level base year data for emissions and GDPpc
		// so that we can adjust base year data to eventually match model data
		currInfo = command.getChild("gdp-varname");
		final ReferenceVariable gdpVarName = (ReferenceVariable)driver.variableList.get(currInfo.getAttributeValue("value"));
		currInfo = command.getChild("emissions-varname");
		final ReferenceVariable emissVarName = (ReferenceVariable)driver.variableList.get(currInfo.getAttributeValue("value"));
		currInfo = command.getChild("emissions-diff-convergence");
		final int emissDiffConvYear = Integer.valueOf(currInfo.getAttributeValue("year"));
		currInfo = command.getChild("debug-country");
		final String debugCountry = currInfo == null ? null : currInfo.getAttributeValue("name");
		final Map<String, Double> GDPpc_r_BY = new HashMap<String, Double>(minicamRegions.size());
		final Map<String, Double> GDPpc_Diff_r_BY = new HashMap<String, Double>(minicamRegions.size());
		final Map<String, Double> E_r_BY = new HashMap<String, Double>(minicamRegions.size());
		final Map<String, Double> E_Diff_r_BY = new HashMap<String, Double>(minicamRegions.size());
		final String baseTimeStr = String.valueOf(BASE_YEAR)+".0";
		final int emissFinalYearIndex = emissTable.getColumnCount()-2;
		final int emissBaseYearIndex = emissTable.findColumn(String.valueOf(BASE_YEAR));
		final int gdpBaseYearIndex = gdpTable.findColumn(String.valueOf(BASE_YEAR));
		final Map<String, Map<String, Double>> shares_GDPpc = new HashMap<String, Map<String, Double>>(minicamRegions.size());
		final Map<String, Map<String, Double>> shares_E = new HashMap<String, Map<String, Double>>(minicamRegions.size());
		if(emissBaseYearIndex == -1 || gdpBaseYearIndex == -1) {
			log.severe("Could not find base year column in tables.");
			return false;
		}
		for(int j = 0; j < populationTable.getRowCount(); ++j) {
			final String currMinicamRegionName = checkRegion((String)populationTable.getValueAt(j, popRegionIndex));
			Region currRegion = null;
			for(Iterator<Region> it = minicamRegions.iterator(); it.hasNext(); ) {
				Region tempRegion = it.next();
				if(tempRegion.name.equals(currMinicamRegionName)) {
					currRegion = tempRegion;
					break;
				}
			}
			assert(currRegion != null);
			final double US90_TO_US95 = 0.001305215;
			double GDPpc_BY_temp = getVariableSumValues(gdpVarName, baseTimeStr,
					currRegion);
			GDPpc_BY_temp /= US90_TO_US95;
			GDPpc_BY_temp /= (Double)populationTable.getValueAt(j, popBaseYearIndex);
			GDPpc_r_BY.put(currMinicamRegionName, GDPpc_BY_temp);
			Map<String, Double> tempMap = new HashMap<String, Double>();
			tempMap.put("total", 0.0);
			shares_GDPpc.put(currMinicamRegionName, tempMap);
			double gdp_diff_BY = (Double)gdpTable.getValueAt(j, gdpBaseYearIndex)
				/ GDPpc_BY_temp;
			GDPpc_Diff_r_BY.put(currMinicamRegionName, gdp_diff_BY);

			// convert from data read to MTC what model uses
			double E_BY_temp = getVariableSumValues(emissVarName, baseTimeStr,
					currRegion);
			E_BY_temp *= emissConvFactor;
			E_r_BY.put(currMinicamRegionName, E_BY_temp);
			tempMap = new HashMap<String, Double>();
			tempMap.put("total", 0.0);
			shares_E.put(currMinicamRegionName, tempMap);

			// if emissDiffConvYear < BASE_YEAR we carry the difference through out
			// all years if emissDiffConvYear == BASE_YEAR we adjust the base year
			// numbers to match model data and if it is > we linearly reduce the 
			// difference until it is all gone in the emissDiffConvYear
			double emiss_diff_BY = 0;
			try {
				emiss_diff_BY = (Double)emissTable.getValueAt(j, emissBaseYearIndex)
					/ E_BY_temp;
			} catch(ClassCastException e) {
				e.printStackTrace();
				System.out.println("Was trying to cast >"+emissTable.getValueAt(j, emissBaseYearIndex)
						+" at row: "+j+" col: "+emissBaseYearIndex);
				throw e;
			}
			E_Diff_r_BY.put(currMinicamRegionName, emiss_diff_BY);

		       if(emissDiffConvYear != BASE_YEAR) {
			       double slope = emissDiffConvYear > BASE_YEAR ? emiss_diff_BY /
				       (emissDiffConvYear - BASE_YEAR) : 0;
			       for(int i = emissBaseYearIndex + 1; i <= emissFinalYearIndex; ++i) {
				       double currDiff = emiss_diff_BY;
				       int currYear = Integer.valueOf(emissTable.getColumnName(i));
				       currDiff -= slope * (currYear - BASE_YEAR);
				       double modelEmiss = (Double)emissTable.getValueAt(j, i);
				       emissTable.setValueAt(modelEmiss - currDiff, j, i);
			       }
		       }
		}

		// calc GDPpc_r_CY
		// calc EI_r_CY 
		// assume the final period is second from end
		final int gdpFinalYearIndex = gdpTable.getColumnCount()-2;
		final int popFinalYearIndex = populationTable.getColumnCount()-2;
		final int finalModelYear = Integer.valueOf(gdpTable.getColumnName(gdpFinalYearIndex));
		assert(finalModelYear < CONV_YEAR);
		// TODO: why should this column name have to be capital?
		final int gdpRegionIndex = gdpTable.findColumn("Region");
		final int emissRegionIndex = emissTable.findColumn("region");
		if(gdpRegionIndex == -1) {
			log.severe("Could not find region column in GDP table.");
			return false;
		}
		if(emissRegionIndex == -1) {
			log.severe("Could not find region column in Emissions table.");
			return false;
		}
		final Map<String, Double> GDPpc_r_CY = new HashMap<String, Double>(minicamRegions.size());
		final Map<String, Double> EI_r_CY = new HashMap<String, Double>(minicamRegions.size());
		for(int j = 0; j < gdpTable.getRowCount(); ++j) {
			final String currMiniCAMRegion = checkRegion((String)gdpTable.getValueAt(j, gdpRegionIndex));
			// TODO: remove this when sure regions are in the same order
			final String testMRegion = checkRegion((String)emissTable.getValueAt(j, emissRegionIndex));
			assert(currMiniCAMRegion.equals(testMRegion));
			// assume constant growth rate of the last period for all periods after 2100
			double GDPpc_gr_r_CY = (Double)gdpTable.getValueAt(j, gdpFinalYearIndex) / 
				(Double)gdpTable.getValueAt(j, gdpFinalYearIndex -1);
			double finalYearTimeStep = Double.valueOf(gdpTable.getColumnName(gdpFinalYearIndex)) - 
				Double.valueOf(gdpTable.getColumnName(gdpFinalYearIndex -1));
			// grow the regional GDP in CY based on eq: (3)
			GDPpc_r_CY.put(currMiniCAMRegion, (Double)gdpTable.getValueAt(j, gdpFinalYearIndex) * 
					Math.pow(GDPpc_gr_r_CY, ((CONV_YEAR - finalModelYear)/finalYearTimeStep)));

			// emissions intensity is emissions / GDP or emissions / (GDPpc * POP)
			double tempEIFinalYear = (Double)emissTable.getValueAt(j, emissFinalYearIndex) / 
				((Double)gdpTable.getValueAt(j, gdpFinalYearIndex) * (Double)populationTable.getValueAt(j,
													popFinalYearIndex));
			double tempEIFinalYear_1 = (Double)emissTable.getValueAt(j, emissFinalYearIndex-1) / 
				((Double)gdpTable.getValueAt(j, gdpFinalYearIndex-1) * (Double)populationTable.getValueAt(j,
													popFinalYearIndex-1));
			// as with GDP it is a constant growth rate from here on out
			// when emissions in the final year or the year before that is zero
			// we need to special case since it will need to converge to 0 before
			// the convergence year. TODO: what is the proper way to do this
			// right now I am just setting the rate at 1 and letting the emissions
			// remainder get split out by gdp per capita
			double EI_gr_r_CY = tempEIFinalYear == 0 || tempEIFinalYear_1 == 0 ?
				1 : tempEIFinalYear / tempEIFinalYear_1;
			// grow the regional EI in CY based on an equation similar to (3)
			EI_r_CY.put(currMiniCAMRegion, tempEIFinalYear * Math.pow(EI_gr_r_CY, 
					((CONV_YEAR - finalModelYear) / finalYearTimeStep)));
			if(currMiniCAMRegion.equals(debugCountry)) {
				System.out.println("GR_CY: "+GDPpc_gr_r_CY);
				System.out.println("GDPpr_r_CY: "+GDPpc_r_CY.get(currMiniCAMRegion));
				System.out.println("GDP F Y: "+gdpTable.getValueAt(j, gdpFinalYearIndex));
				System.out.println("Time Step: "+finalYearTimeStep);
				System.out.println("EI_gr_r_CY: "+EI_gr_r_CY);
				System.out.println("EI_r_CY: "+EI_r_CY.get(currMiniCAMRegion));
			}
		}


		// initialize the GDP and EI base year country data
		// TODO: I should be able to put this check back in now that
		// the variables are switched to DM variables, but should test
		// it first
		/* TODO: figure out how to check and see if the the vars exist
		 * and are defined for the BASE_YEAR. The following checks user's
		 * DM variables (not from PP).
		if(!driver.variableList.containsKey(gdpVarName)) {
			log.severe("Could not find GDP variable: "+gdpVarName);
			return false;
		}
		*/
		final Set<String> countryNames = POPc.get(BASE_YEAR).keySet();
		final Map<String, Double> GDPpc_BY = new HashMap<String, Double>(countryNames.size());
		final Map<String, Double> GDPpc_grc = new HashMap<String, Double>(countryNames.size());
		final Map<String, Double> E_c_BY = new HashMap<String, Double>(countryNames.size());
		final Map<String, Double> EI_c_BY = new HashMap<String, Double>(countryNames.size());
		final Map<String, Double> EI_grc = new HashMap<String, Double>(countryNames.size());
		for(Iterator<String> it = countryNames.iterator(); it.hasNext(); ) {
			final String currCountry = it.next();
			// convert from 95US$ (data read) to 90US$/person what model uses
			final double US90_TO_US95 = 0.001305215;
			double GDPpc_BY_temp = getVariableSumValues(gdpVarName, baseTimeStr,
					(Region)driver.regionList.get(currCountry));
			GDPpc_BY_temp /= US90_TO_US95;
			GDPpc_BY_temp /= POPc.get(BASE_YEAR).get(currCountry);

			double E_c_BY_temp = getVariableSumValues(emissVarName, baseTimeStr,
					(Region)driver.regionList.get(currCountry));
			E_c_BY_temp *= emissConvFactor;
			if(GDPpc_BY_temp != 0.0 && E_c_BY_temp != 0.0) {
				Region currMiniCAMRegion = getSuperRegion(minicamRegions, currCountry);

				// TODO: make it so it is never null
				if(currMiniCAMRegion != null) {
					// calculate base year shares for GDPpc
					double tempShare = GDPpc_BY_temp / GDPpc_r_BY.get(currMiniCAMRegion.name);
					Map<String, Double> tempMap = shares_GDPpc.get(currMiniCAMRegion.name);
					tempMap.put(currCountry, tempShare);
					tempMap.put("total", tempMap.get("total") + tempShare);

					// calculate base year shares for E only if emissDiffConvYear == BASE_YEAR
					if(emissDiffConvYear == BASE_YEAR) {
						tempShare = E_c_BY_temp / E_r_BY.get(currMiniCAMRegion.name);
						tempMap = shares_E.get(currMiniCAMRegion.name);
						tempMap.put(currCountry, tempShare);
						tempMap.put("total", tempMap.get("total") + tempShare);
					}
				}
			}
			GDPpc_BY.put(currCountry, GDPpc_BY_temp);
			E_c_BY.put(currCountry, E_c_BY_temp);
		}

		// share out the base year difference between read in data and
		// model data
		normalizeAndDistribute(GDPpc_BY, shares_GDPpc, GDPpc_Diff_r_BY);
		if(emissDiffConvYear == BASE_YEAR) {
			normalizeAndDistribute(E_c_BY, shares_E, E_Diff_r_BY);
		}

		// we can calc growth rates and intensities after differences have been
		// distributed
		for(Iterator<String> it = countryNames.iterator(); it.hasNext(); ) {
			final String currCountry = it.next();
			double GDPpc_BY_temp = GDPpc_BY.get(currCountry);
			double GDPpc_grc_temp = 0.0;
			double E_c_BY_temp = E_c_BY.get(currCountry);
			double EI_grc_temp = 0.0;
			double EI_c_BY_temp = 0.0;
			if(GDPpc_BY_temp != 0.0 && E_c_BY_temp != 0.0) {
				Region currMiniCAMRegion = getSuperRegion(minicamRegions, currCountry);

				// TODO: make it so it is never null
				if(currMiniCAMRegion != null) {
					// calculate eq: (2)
					// note that this is an anual growth rate
					GDPpc_grc_temp = Math.pow((GDPpc_r_CY.get(currMiniCAMRegion.name) / GDPpc_BY_temp),
						(1.0 / (CONV_YEAR - BASE_YEAR)));
					// convert to E to EI
					EI_c_BY_temp = E_c_BY_temp / (GDPpc_BY_temp * POPc.get(BASE_YEAR).get(currCountry)); 
					// calculate eq: (8)
					// again this is an anual growth rate
					EI_grc_temp = Math.pow((EI_r_CY.get(currMiniCAMRegion.name) / EI_c_BY_temp),
						(1.0 / (CONV_YEAR - BASE_YEAR)));

					// No change in pattern if CONV_YEAR is larger than max value to gaurd against infinity
					if(CONV_YEAR > MAX_CONV_YEAR) {
						// reset these to 1 to avoid possible inifinites
						GDPpc_grc_temp = 1;
						EI_grc_temp = 1;
					}
					if(currCountry.equals(debugCountry)) {
						System.out.println("C: "+currCountry);
						System.out.println("grc: "+GDPpc_grc_temp);
						System.out.println("CBY: "+GDPpc_BY_temp);
						System.out.println("RCY: "+GDPpc_r_CY.get(currMiniCAMRegion.name));
						System.out.println("EI_grc: "+EI_grc_temp);
						System.out.println("EI_cBY: "+EI_c_BY_temp);
						System.out.println("EI_RCY: "+EI_r_CY.get(currMiniCAMRegion.name));
					}
				}
			}
			GDPpc_grc.put(currCountry, GDPpc_grc_temp);
			EI_c_BY.put(currCountry, EI_c_BY_temp);
			EI_grc.put(currCountry, EI_grc_temp);
		}

		// calculate GDPDiff_rt, GDP_sh_ct, and finally GDPpc_ct
		int currYear = BASE_YEAR;
		int yearBefore;
		int gdpYearIndex = gdpTable.findColumn(String.valueOf(BASE_YEAR));
		popIndex = populationTable.findColumn(String.valueOf(BASE_YEAR));
		if(gdpYearIndex == -1 || popIndex == -1) {
			log.severe("Could not find base year column in tables.");
			return false;
		}
		final Map<Integer, Map<String, Double>> GDPpc_ct = new HashMap<Integer, Map<String, Double>>(calcYears.size());
		Map<String, Double> prevGDPpc_ct;
		Map<String, Double> currGDPpc_ct = GDPpc_BY;
		for(Iterator<Integer> itTime = calcYears.iterator(); itTime.hasNext(); ) {
			yearBefore = currYear;
			currYear = itTime.next();
			prevGDPpc_ct = currGDPpc_ct;
			if(currYear == BASE_YEAR) {
				GDPpc_ct.put(BASE_YEAR, GDPpc_BY);
				continue;
			}
			Map<String, Double> POPc_t = POPc.get(currYear);
			currGDPpc_ct = new HashMap<String, Double>(POPc_t.size());
			Map<String, Double> currGDPDiff_rt = new HashMap<String, Double>(minicamRegions.size());
			Map<String, Double> currDiff = new HashMap<String, Double>(minicamRegions.size());
			Map<String, Double> currShare = new HashMap<String, Double>(minicamRegions.size());
			// the final GDPpc_ct the other is the *
			Map<String, Double> currGDPpc_ctF = new HashMap<String, Double>(POPc_t.size());
			GDPpc_ct.put(currYear, currGDPpc_ctF);
			// move the column down to the next year
			++gdpYearIndex;
			++popIndex;
			// this assumes same region ordering for POP and GDP tables
			for(int j = 0; j < gdpTable.getRowCount(); ++j) {
				String currMiniCAMRegion = checkRegion((String)gdpTable.getValueAt(j, gdpRegionIndex));
				currDiff.put(currMiniCAMRegion, 0.0);
				currShare.put(currMiniCAMRegion, 0.0);
				currGDPDiff_rt.put(currMiniCAMRegion, (Double)gdpTable.getValueAt(j, gdpYearIndex) * 
					(Double)populationTable.getValueAt(j, popIndex));
			}
			for(Iterator<Map.Entry<String, Double>> itPOPc = POPc_t.entrySet().iterator(); itPOPc.hasNext(); ) {
				Map.Entry<String, Double> currPOPc = itPOPc.next();
				String currC = currPOPc.getKey();
				// eq: (3)
				// we have to apply the anual growth rate for each year in the time step.
				// TODO: calculate the time step rather than assuming it is 15 years
				currGDPpc_ct.put(currC, prevGDPpc_ct.get(currC)*Math.pow(GDPpc_grc.get(currC), 15.0));
				Region tempR = getSuperRegion(minicamRegions, currC);
				// TODO: make sure this never happens
				if(tempR == null) {
					continue;
				}
				String minicamSuperRegion = tempR.name;
				// sum part of eq: (4)
				double temp = currDiff.get(minicamSuperRegion);
				temp += currGDPpc_ct.get(currC)*currPOPc.getValue();
				double tempReport = temp;
				currDiff.put(minicamSuperRegion, temp);
				// sum part of eq: (5)
				temp = currShare.get(minicamSuperRegion);
				double temp2 = currGDPpc_ct.get(currC)*currPOPc.getValue() -
					prevGDPpc_ct.get(currC)*POPc.get(yearBefore).get(currC);
				if(minicamSuperRegion.equals("debugCountry")) {
					System.out.println(currC+" going neg");
					System.out.println("pc_ct* : "+currGDPpc_ct.get(currC));
					System.out.println("POPct: "+currPOPc.getValue());
					System.out.println("pc_ct-1: "+prevGDPpc_ct.get(currC));
					System.out.println("POPct-1: "+POPc.get(yearBefore).get(currC));
					System.out.println("tmep2: "+temp2);
					System.out.println("GR: "+GDPpc_grc.get(currC));
					System.out.println("Curr summed diff: "+tempReport);
				}
				temp += temp2;
				currShare.put(minicamSuperRegion, temp);
			}
			// finish calc for GDP Diff_rt
			for(Iterator<Region> itR = minicamRegions.iterator(); itR.hasNext(); ) {
				String currMiniCAMRegion = itR.next().name;
				// finished eq: (4)
				double temp = currGDPDiff_rt.get(currMiniCAMRegion);
				temp -= currDiff.get(currMiniCAMRegion);
				currGDPDiff_rt.put(currMiniCAMRegion, temp);
			}
			// finish calc for GDP_sh_ct and GDPpc_ct
			for(Iterator<Map.Entry<String, Double>> itPOPc = POPc_t.entrySet().iterator(); itPOPc.hasNext(); ) {
				Map.Entry<String, Double> currPOPc = itPOPc.next();
				String currC = currPOPc.getKey();
				// finished eq: (5)
				double tempShare = currGDPpc_ct.get(currC)*currPOPc.getValue() -
					prevGDPpc_ct.get(currC)*POPc.get(yearBefore).get(currC);
				Region tempR = getSuperRegion(minicamRegions, currC);
				// TODO: make sure this never happens
				if(tempR == null) {
					currGDPpc_ctF.put(currC, 0.0);
					continue;
				}
				String minicamSuperRegion = tempR.name;
				tempShare /= currShare.get(minicamSuperRegion);
				// eq: (6)
				double temp = currGDPpc_ct.get(currC) + ((currGDPDiff_rt.get(minicamSuperRegion) * tempShare) / 
					currPOPc.getValue());
				currGDPpc_ctF.put(currC, temp);
				if(currC.equals(debugCountry)) {
					System.out.println("tempShare: "+tempShare);
					System.out.println("R Diff: "+currGDPDiff_rt.get(minicamSuperRegion));
					System.out.println("currGDPpc_ct: "+temp);
					System.out.println("currGDPpc_ct*: "+currGDPpc_ct.get(currC));
					System.out.println("GR: "+GDPpc_grc.get(currC));
				}
			}
		}


		// calc E_ct by calculating preliminary value, and adjusting it by the countries share
		// of the regional difference
		int emissYearIndex = emissTable.findColumn(String.valueOf(BASE_YEAR));
		if(emissYearIndex == -1) {
			log.severe("Could not find base year column in emissions table.");
			return false;
		}
		final Map<Integer, Map<String, Double>> E_ct = new HashMap<Integer, Map<String, Double>>(calcYears.size());
		Map<String, Double> currEI_s_ct = null;
		Map<String, Double> prevEI_s_ct = null;
		for(Iterator<Integer> itTime = calcYears.iterator(); itTime.hasNext(); ) {
			currYear = itTime.next();
			prevEI_s_ct = currEI_s_ct;
			if(currYear == BASE_YEAR) {
				E_ct.put(currYear, E_c_BY);
				currEI_s_ct = EI_c_BY;
				if(CONV_YEAR > MAX_CONV_YEAR) {
					// we will need the regional sum to back out appropriate base year emissions shares
					// the emissions here would have already been adjusted to match base year data if
					// the emissions convergence year was != BASE_YEAR
					for(int j = 0; j < emissTable.getRowCount(); ++j) {
						String currMiniCAMRegion = checkRegion((String)emissTable.getValueAt(j, emissRegionIndex));
						E_r_BY.put(currMiniCAMRegion, (Double)emissTable.getValueAt(j, emissYearIndex));
					}
				}
				continue;
			}
			Map<String, Double> POPc_t = POPc.get(currYear);
			Map<String, Double> currEmissDiff_rt = new HashMap<String, Double>(minicamRegions.size());
			Map<String, Double> currEmissRegionalSum = new HashMap<String, Double>(minicamRegions.size());
			// the preliminary EI*
			currEI_s_ct= new HashMap<String, Double>(POPc_t.size());
			Map<String, Double> currE_ct= new HashMap<String, Double>(POPc_t.size());
			E_ct.put(currYear, currE_ct);
			++emissYearIndex;
			for(int j = 0; j < emissTable.getRowCount(); ++j) {
				String currMiniCAMRegion = checkRegion((String)emissTable.getValueAt(j, emissRegionIndex));
				currEmissRegionalSum.put(currMiniCAMRegion, 0.0);
				currEmissDiff_rt.put(currMiniCAMRegion, (Double)emissTable.getValueAt(j, emissYearIndex));
			}
			// calc EI*_ct the preliminary E_ct and the regional emission sums
			for(Iterator<Map.Entry<String, Double>> itPOPc = POPc_t.entrySet().iterator(); itPOPc.hasNext(); ) {
				Map.Entry<String, Double> currPOPc = itPOPc.next();
				String currC = currPOPc.getKey();
				Region tempR = getSuperRegion(minicamRegions, currC);
				// TODO: make sure this never happens
				if(tempR == null) {
					continue;
				}
				// equation similar to (3)
				// we have to apply the anual growth rate for each year in the time step.
				// TODO: calculate the time step rather than assuming it is 15 years
				double EI_s_ct_temp = prevEI_s_ct.get(currC)*Math.pow(EI_grc.get(currC), 15.0);
				currEI_s_ct.put(currC, EI_s_ct_temp);
				// calculate preliminary E_ct with eq: (7)
				double currGDPpc = GDPpc_ct.get(currYear).get(currC); 
				double preliminaryE_ct = E_c_BY.get(currC) * (currPOPc.getValue() / POPc.get(BASE_YEAR).get(currC)) *
						(currGDPpc / GDPpc_ct.get(BASE_YEAR).get(currC)) *
						(EI_s_ct_temp / EI_c_BY.get(currC));
				currE_ct.put(currC, preliminaryE_ct);
				String minicamSuperRegion = tempR.name;
				// sum the regional emissions used in calc for EDiff_r and E_sh_ct see eq: (9)
				double tempSum = currEmissRegionalSum.get(minicamSuperRegion);
				tempSum += currGDPpc * currPOPc.getValue() * EI_s_ct_temp;
				currEmissRegionalSum.put(minicamSuperRegion, tempSum);
			}
			// finish calc for Emissions Diff_rt
			for(Iterator<Region> itR = minicamRegions.iterator(); itR.hasNext(); ) {
				String currMiniCAMRegion = itR.next().name;
				// finished equation similar to (4)
				double temp = currEmissDiff_rt.get(currMiniCAMRegion);
				temp -= currEmissRegionalSum.get(currMiniCAMRegion);
				currEmissDiff_rt.put(currMiniCAMRegion, temp);
			}
			// finish calc for E_sh_ct and E_ct
			for(Iterator<Map.Entry<String, Double>> itPOPc = POPc_t.entrySet().iterator(); itPOPc.hasNext(); ) {
				Map.Entry<String, Double> currPOPc = itPOPc.next();
				String currC = currPOPc.getKey();
				Region tempR = getSuperRegion(minicamRegions, currC);
				// TODO: make sure this never happens
				if(tempR == null) {
					currE_ct.put(currC, 0.0);
					continue;
				}
				String minicamSuperRegion = tempR.name;
				// finished eq: (9)
				double tempShare = GDPpc_ct.get(currYear).get(currC) * currPOPc.getValue() *
					currEI_s_ct.get(currC);
				tempShare /= currEmissRegionalSum.get(minicamSuperRegion);
				// eq: (10)
				double temp = currE_ct.get(currC) + (currEmissDiff_rt.get(minicamSuperRegion) * tempShare);
				if(CONV_YEAR > MAX_CONV_YEAR) {
					// if the CONV_YEAR is farther out than the MAX_CONV_YEAR
					// we assume there is no convergence in emissions so just
					// multiply the current emissions by the base year shares
					// we reset E_r_BY and E_c_BY should have also been adjusted
					// if necessary for the emissions difference convergence,
					// dividing them should give us the appropriate shares
					// or the base year country emissions E_c_BY have been adjusted so we should be good
					temp = (currEmissDiff_rt.get(minicamSuperRegion) + currEmissRegionalSum.get(minicamSuperRegion))
							* (E_c_BY.get(currC) / E_r_BY.get(minicamSuperRegion));
					if(currC.equals(debugCountry)) {
						System.out.println("Diff_rt: "+currEmissDiff_rt.get(minicamSuperRegion));
						System.out.println("Reg Sum: "+currEmissRegionalSum.get(minicamSuperRegion));
						System.out.println("E_c_BY: "+E_c_BY.get(currC));
						System.out.println("E_r_BY: "+E_r_BY.get(minicamSuperRegion));
					}
				}
				currE_ct.put(currC, temp);
			}
		}

		currInfo = command.getChild("emissions-csv-out");
		if(currInfo != null) {
			try {
				dumpMapToCSV(E_ct.entrySet().iterator(), currInfo.getAttributeValue("file"));
			} catch(Exception e) {
				log.warning("Could not write emissions csv file: "+e);
			}
		}
		currInfo = command.getChild("population-csv-out");
		if(currInfo != null) {
			try {
				dumpMapToCSV(POPc.entrySet().iterator(), currInfo.getAttributeValue("file"));
			} catch(Exception e) {
				log.warning("Could not write population csv file: "+e);
			}
		}
		currInfo = command.getChild("gdp-csv-out");
		if(currInfo != null) {
			try {
				dumpMapToCSV(GDPpc_ct.entrySet().iterator(), currInfo.getAttributeValue("file"));
			} catch(Exception e) {
				log.warning("Could not write gdp csv file: "+e);
			}
		}
		currInfo = command.getChild("emissions-out-varname");
		createOutputVar(currInfo.getAttributeValue("value"), E_ct, emissVarName, baseTimeStr, driver, emissConvFactor);
		return true;
	}

	/**
	 * Dumps a map which is as the form time - country - value as a CSV table. 
	 * This is used to debug. The results will be placed in the given filename.
	 * @param it and iterator that iterates through times for each country.
	 * @param file The name of a file to place results into. 
	 */
	private static void dumpMapToCSV(Iterator<Map.Entry<Integer, Map<String, Double>>> it, String file) throws Exception {
		boolean isFirst = true;
		final PrintStream out = new PrintStream(new File(file));
		Map.Entry<Integer, Map<String, Double>> currCountryMap = null;
		for(Iterator<Map.Entry<Integer, Map<String, Double>>> itTime = it; itTime.hasNext(); ) {
			if(!isFirst && currCountryMap != null) {
				// every other time
				currCountryMap = itTime.next();
				out.print(currCountryMap.getKey()+",");
			} else if(isFirst && currCountryMap == null) {
				// first time
				currCountryMap = itTime.next();
				out.print(",");
			} else {
				// second time
				isFirst = false;
				out.print(currCountryMap.getKey()+",");
			}
			for(Iterator<Map.Entry<String, Double>> itC 
					= currCountryMap.getValue().entrySet().iterator(); itC.hasNext(); ) {
				Map.Entry<String, Double> currCountry = itC.next();
				if(isFirst) {
					out.print(currCountry.getKey()+",");
				} else {
					out.print(currCountry.getValue()+",");
				}
			}
			out.println();
		}
		out.close();
	}

	/**
	 * Gets data from the model output by running a query against an xml
	 * database.
	 * @param query The element who's child is the query to be run.
	 * @param scnarioList A list with the scnearios to look in, there should only be one.
	 * @param regionList A list with the region names to look in.  Usually just global. 
	 * @return A table that is created by running the query
	 * @throws Exception If there was an error while building the query or table.
	 */
	private static BaseTableModel getModelData(Element query, Object[] scenarioList, Object[] regionList) throws Exception {
		assert(XMLDB.getInstance() != null);
		assert(scenarioList.length == 1);
		// have to get the query and convert it to the standard org.w3c.dom.Element 
		// which is what the query generator expects
		final Element queryChild = (Element)query.getChildren().get(0);
		queryChild.detach();
		final DOMOutputter domConverter = new DOMOutputter();
		final QueryGenerator qg = new QueryGenerator(domConverter.output(new Document(queryChild)).getDocumentElement());
		assert(qg.isValid());
		// could get complicated when trying to get data from tables
		assert(!qg.isGroup());
		return new ComboTableModel(qg, scenarioList, regionList, null, null, null);
	}

	/**
	 * Gets the sum of the values for the varName in the region.  If the region is null
	 * or the varName in the region is not found 0 will be returned. Note that this assumes 
	 * the value has avg = false.
	 * @param varName The variable to sum.
	 * @param year The year to look for. 
	 * @param region The acutal region to look in. 
	 * @return The summed value of varName in region for year. 
	 */
	private static double getVariableSumValues(ReferenceVariable varName, String year, Region region) {
		if(region == null) {
			// TODO: log this?
			return 0.0;
		}

		Wrapper[] ret;
		if(varName.avg) {
			// need to weight values
			// I don't think the x/y/h are necessary anymore
			ret = ComponentManipulator.sumValues(region.extractRegion(varName), varName.weight, varName.getLandFract(), varName.x,
					varName.y, varName.h);
		} else {
			ret = ComponentManipulator.sumValues(region.extractRegion(varName));
		}
		return ret[0].getData()[0][0];
	}

	/**
	 * Gets the Region that contains the countryName.  If no Region can be found
	 * null is returned.  Note by Super region we really mean MiniCAM region.
	 * @param minicamRegions The list of regions.
	 * @param countryName The country name to search for.
	 * @return The Region that contains countryName.
	 */
	private static Region getSuperRegion(List<Region> minicamRegions, String countryName) {
		for(Iterator<Region> it = minicamRegions.iterator(); it.hasNext(); ) {
			Region currMiniCAMRegion = it.next();
			if(currMiniCAMRegion.containsRegion(countryName)) {
				return currMiniCAMRegion;
			}
		}
		return null;
	}

	/**
	 * Distributes the regional difference between the readin data and the
	 * model data base on shares.  The shares are first normalized which
	 * assumes that there is a value "total" in the shares which has the share 
	 * sum already calculated.
	 * @param countryData The country level data that needs to be adjusted.
	 * @param shares A map of Region to country to it's share.  It is assumed
	 * 	that for each country there is a "total" which as the share sum
	 * 	already calculated for that region.
	 * @param regionalDiff The difference to be distributed from the region
	 * 	level to the country level.
	 */
	private static void normalizeAndDistribute(Map<String, Double> countryData,
			Map<String, Map<String, Double>> shares,
			Map<String, Double> regionalDiff) {
		for(Iterator<Map.Entry<String, Map<String, Double>>> it = shares.entrySet().iterator();
				it.hasNext(); ) {
			Map.Entry<String, Map<String, Double>> currRegionShares = it.next();
			double currRegionDiff = regionalDiff.get(currRegionShares.getKey());
			Map<String, Double> currShares = currRegionShares.getValue();
			double currTotalShare = currShares.get("total");
			for(Iterator<Map.Entry<String, Double>> itShares = currShares.entrySet().iterator();
					itShares.hasNext(); ) {
				Map.Entry<String, Double> currCountryShare = itShares.next();
				String currC = currCountryShare.getKey();
				if(currC.equals("total")) {
					continue;
				}
				countryData.put(currC, countryData.get(currC) *
						/* ((currCountryShare.getValue() / currTotalShare)) * */
						 currRegionDiff);
			}
		}
	}

	/**
	 * Creates the downscaled dm var which will be grouped by time.  The process goes by 
	 * taking the base year map and scaling it based on the % change from the base year 
	 * country sum to the downscaled country values.  It then takes that % and scales
	 * each grid cell in the country by it.  Note that it uses convFactor such that the
	 * results are left in the same units as the base year map.
	 * @param outVarName The name of the group variable that will be created.
	 * @param downscaledData The data map of time to country to data.
	 * @param inVarName The base year data var. 
	 * @param baseYear The base year to start from.
	 * @param driver The manipulator driver which will be used to get access to Regions.
	 * @param convFactor Conversion factor to convert base year units to working units.
	 */
	private static void createOutputVar(String outVarName, Map<Integer, Map<String, Double>> downscaledData,
			ReferenceVariable inVarName, String baseYear, ManipulationDriver driver, double convFactor) {
		// to avoid summing base year values too much we will do it once and store it in the map
		Map<String, Double> baseYearSumCache = new HashMap<String, Double>();

		// set up the out var as a group variable by time
		GroupVariable outVar = new GroupVariable(outVarName);
		outVar.isRef = true;
		outVar.isTime = true;
		driver.variableList.put(outVarName, outVar);
		//String units = (String)driver.dataUnits.get(inVarName);
		String units = inVarName.units;
		final Region worldRegion = (Region)driver.regionList.get("World");
		for(Iterator<Map.Entry<Integer, Map<String, Double>>> itTime = downscaledData.entrySet().iterator(); itTime.hasNext(); ) {
			Map.Entry<Integer, Map<String, Double>> currTime = itTime.next();
			// TODO: check the avg, I would hope I could get the right time here
			ReferenceVariable outCurrYear = new ReferenceVariable(currTime.getKey().toString(), inVarName);
			// this is confusing but copying a reference variable does not copy the data
			// I think for us just copying the data wrapper and setting each pointer in there to the same as in
			// inVarName should be good enough
			outCurrYear.data = new Wrapper[inVarName.data.length];
			for(int i = 0; i < inVarName.data.length; ++i) {
				outCurrYear.data[i] = inVarName.data[i].makeCopy();
				outCurrYear.data[i].setData(inVarName.data[i].getData());
			}
			outCurrYear.units = units;
			outVar.addData(outCurrYear);
			driver.variableList.put(outVarName+outCurrYear.name, outCurrYear);
			Map<String, Double> countryMap = currTime.getValue();
			for(Iterator<Map.Entry<String, Double>> itCountry = countryMap.entrySet().iterator(); itCountry.hasNext(); ) {
				Map.Entry<String, Double> currCountryData = itCountry.next();
				Region currRegion = (Region)driver.regionList.get(currCountryData.getKey());
				if(currRegion == null) {
					// this means that regions that did not match up will never have their emissions
					// scale from the base year map
					continue;
				}
				Wrapper[] workingVar = currRegion.extractRegion(outCurrYear);
				double countrySum;
				if(baseYearSumCache.containsKey(currCountryData.getKey())) {
					countrySum = baseYearSumCache.get(currCountryData.getKey());
				} else {
					// Multiply by the conversion factor here so that the units cancel with the calculated
					// units.  This will leave us with the units of the base year map.
					countrySum = ComponentManipulator.sumValues(workingVar)[0].getData()[0][0] * convFactor;
					baseYearSumCache.put(currCountryData.getKey(), countrySum);
				}
				double currRatio = currCountryData.getValue() / countrySum;
				Wrapper[] ret = ComponentManipulator.multiplyVar(workingVar, currRatio);
				assert(workingVar.length == ret.length);
				for(int i = 0; i < workingVar.length; ++i) {
					workingVar[i].setData(ret[i].getData());
				}
			}
		}
	}

	/**
	 * Checks if the model used minicam region name should be renamed to match
	 * the data manipulator.  Currently this only matters for China since the DM
	 * has a superRegion China/CPA which is what we want as well as the country China
	 * so we must rename the model value to be China/CPA.
	 * @param regionName The model region name.
	 * @return The appropriate region name to use.
	 */
	private static String checkRegion(String regionName) {
		return regionName.equals("China") ? "China/CPA" : regionName;
	}
}
