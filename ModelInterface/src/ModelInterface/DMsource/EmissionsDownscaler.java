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
 * parts of the ModelInterface and be easily removed is necessary.
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
		//log class to use for all logging output
		final Logger log = Logger.getLogger("DataManipulation"); 
		Element currInfo;
		// TODO: get base year
		final int BASE_YEAR = 2005;
		currInfo = command.getChild("convergent-year");
		final int CONV_YEAR = Integer.valueOf(currInfo.getAttributeValue("value"));
		final List<Region> minicamRegions = ((superRegion)driver.regionList.get("World-MiniCAM")).data;

		// Initialize and get data from the xmld database that has the model's
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
				regionList[i] = minicamRegions.get(i).name;
			}

			XMLDB.openDatabase(dbLocation, null);
			populationTable = getModelData(command.getChild("populationQuery"), 
					scenarioList, regionList);
			gdpTable = getModelData(command.getChild("gdpQuery"), 
					scenarioList, regionList);
			emissTable = getModelData(command.getChild("emissionsQuery"),
					scenarioList, regionList);
			/*
			javax.swing.JDialog jd = new javax.swing.JDialog();
			javax.swing.JTable tbl = new javax.swing.JTable(emissTable);
			javax.swing.JScrollPane sp = new javax.swing.JScrollPane(tbl);
			jd.setContentPane(sp);
			jd.pack();
			jd.setVisible(true);
			*/
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
		// TODO: figure out if closing will cause the tables to get errors

		// initialize the country population data.
		// the population by country stored as a Map of Time -> Region -> Data
		final Map<Integer, Map<String, Double>> POPc = new /*java.util.Linked*/HashMap<Integer, Map<String, Double>>();
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
					new /*java.util.Linked*/HashMap<String, Double>(currLineSplit.length-1);
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
				final String currMinicamRegionName = (String)populationTable.getValueAt(j, popRegionIndex);
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
						/*
						if(currMinicamRegionName.equals("Canada")) {
							System.out.println("POPr: "+populationTable.getValueAt(j, popIndex));
							System.out.println("Ac: "+currCountry.getValue());
							System.out.println("Ar: "+currRegionMap.get(currMinicamRegionName));
							System.out.println("POPc: "+POPc_temp);
						}
						*/
						currCountry.setValue(POPc_temp);
					}
				}
			}
			++popIndex;
		}

		// calc GDPpc_r_CY
		// calc EI_r_CY 
		// assume the final period is second from end
		final int gdpFinalYearIndex = gdpTable.getColumnCount()-2;
		final int emissFinalYearIndex = emissTable.getColumnCount()-2;
		final int popFinalYearIndex = populationTable.getColumnCount()-2;
		final int finalModelYear = Integer.valueOf(gdpTable.getColumnName(gdpFinalYearIndex));
		assert(finalModelYear < CONV_YEAR);
		// TODO: why should this column name have to be capital?
		// TODO: figure out what I ment by the above todo
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
			final String currMiniCAMRegion = (String)gdpTable.getValueAt(j, gdpRegionIndex);
			// TODO: remove this when sure regions are in the same order
			final String testMRegion = (String)emissTable.getValueAt(j, emissRegionIndex);
			assert(currMiniCAMRegion.equals("testMRegion"));
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
			double EI_gr_r_CY = tempEIFinalYear / tempEIFinalYear_1;
			// grow the regional EI in CY based on an equation similar to (3)
			EI_r_CY.put(currMiniCAMRegion, tempEIFinalYear * Math.pow(EI_gr_r_CY, 
					((CONV_YEAR - finalModelYear) / finalYearTimeStep)));
			if(currMiniCAMRegion.equals("USA")) {
				System.out.println("GR_CY: "+GDPpc_gr_r_CY);
				System.out.println("GDPpr_r_CY: "+GDPpc_r_CY.get(currMiniCAMRegion));
				System.out.println("GDP F Y: "+gdpTable.getValueAt(j, gdpFinalYearIndex));
				System.out.println("Time Step: "+finalYearTimeStep);
				System.out.println("EI_gr_r_CY: "+EI_gr_r_CY);
				System.out.println("EI_r_CY: "+EI_r_CY.get(currMiniCAMRegion));
			}
		}


		// initialize the GDP and EI base year country data
		currInfo = command.getChild("gdp-varname");
		final String gdpVarName = currInfo.getAttributeValue("value");
		currInfo = command.getChild("emissions-varname");
		final String emissVarName = currInfo.getAttributeValue("value");
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
		final String baseTimeStr = String.valueOf(BASE_YEAR)+".0";
		for(Iterator<String> it = countryNames.iterator(); it.hasNext(); ) {
			final String currCountry = it.next();
			/*
			GDPpc_BY.put(currCountry, getVariableSumValues(gdpVarName, baseTimeStr, 
						(Region)driver.regionList.get(currCountry)));
						*/
			// convert from 95US$ (data read) to 90US$/person what model uses
			final double US90_TO_US95 = 0.001305215;
			double GDPpc_BY_temp = getVariableSumValues(gdpVarName, baseTimeStr,
					(Region)driver.regionList.get(currCountry));
			GDPpc_BY_temp /= US90_TO_US95;
			GDPpc_BY_temp /= POPc.get(BASE_YEAR).get(currCountry);
			double GDPpc_grc_temp = 0.0;

			// convert from GgCO2 (data read) to MTC what model uses
			//final double GgCO2_TO_MTC = 0.000272916;
			final double GgCO2_TO_MTC = 0.000000000272916;
			double E_c_BY_temp = getVariableSumValues(emissVarName, baseTimeStr,
					(Region)driver.regionList.get(currCountry));
			E_c_BY_temp *= GgCO2_TO_MTC;
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
					//if(currMiniCAMRegion.name.equals("Western Europe")) {
					if(currCountry.equals("USA")) {
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
			GDPpc_BY.put(currCountry, GDPpc_BY_temp);
			GDPpc_grc.put(currCountry, GDPpc_grc_temp);
			E_c_BY.put(currCountry, E_c_BY_temp);
			EI_c_BY.put(currCountry, EI_c_BY_temp);
			EI_grc.put(currCountry, EI_grc_temp);
			/*
			if(currCountry.equals("Canada")) {
				System.out.println("Base GDP: "+GDPpc_BY.get(currCountry));
			}
			*/
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
				String currMiniCAMRegion = (String)gdpTable.getValueAt(j, gdpRegionIndex);
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
				/*
				if(minicamSuperRegion.equals("USA")) {
					System.out.println(currC+" going neg");
					System.out.println("pc_ct* : "+currGDPpc_ct.get(currC));
					System.out.println("POPct: "+currPOPc.getValue());
					System.out.println("pc_ct-1: "+prevGDPpc_ct.get(currC));
					System.out.println("POPct-1: "+POPc.get(yearBefore).get(currC));
					System.out.println("tmep2: "+temp2);
					System.out.println("GR: "+GDPpc_grc.get(currC));
					System.out.println("Curr summed diff: "+tempReport);
				}
				*/
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
				/*
				if(currC.equals("United Kingdom")) {
					System.out.println("tempShare before: "+tempShare);
				}
				*/
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
				if(currC.equals("Cameroon")) {
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
				String currMiniCAMRegion = (String)emissTable.getValueAt(j, emissRegionIndex);
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
				/*
				if(currC.equals("United Kingdom")) {
					System.out.println("tempShare before: "+tempShare);
				}
				*/
				tempShare /= currEmissRegionalSum.get(minicamSuperRegion);
				// eq: (10)
				double temp = currE_ct.get(currC) + (currEmissDiff_rt.get(minicamSuperRegion) * tempShare);
				currE_ct.put(currC, temp);
				if(currC.equals("USA")) {
					System.out.println("E tempShare: "+tempShare);
					System.out.println("E R Diff: "+currEmissDiff_rt.get(minicamSuperRegion));
					System.out.println("currE_ct: "+temp);
				}
			}
		}

		//dumpMapToCSV(E_ct.entrySet().iterator());
		//dumpMapToCSV(GDPpc_ct.entrySet().iterator());
		dumpMapToCSV(POPc.entrySet().iterator());
		return true;
	}

	/**
	 * Dummps a map which is as the form time - country - value as a CSV table. 
	 * This is used to debug.
	 * @param it and iterator that iterates through times for each country.
	 */
	private static void dumpMapToCSV(Iterator<Map.Entry<Integer, Map<String, Double>>> it) {
		boolean isFirst = true;
		Map.Entry<Integer, Map<String, Double>> currCountryMap = null;
		for(Iterator<Map.Entry<Integer, Map<String, Double>>> itTime = it; itTime.hasNext(); ) {
			if(!isFirst && currCountryMap != null) {
				// every other time
				currCountryMap = itTime.next();
				System.out.print(currCountryMap.getKey()+",");
			} else if(isFirst && currCountryMap == null) {
				// first time
				currCountryMap = itTime.next();
				System.out.print(",");
			} else {
				// second time
				isFirst = false;
				System.out.print(currCountryMap.getKey()+",");
			}
			for(Iterator<Map.Entry<String, Double>> itC 
					= currCountryMap.getValue().entrySet().iterator(); itC.hasNext(); ) {
				Map.Entry<String, Double> currCountry = itC.next();
				if(isFirst) {
					System.out.print(currCountry.getKey()+",");
				} else {
					System.out.print(currCountry.getValue()+",");
				}
			}
			System.out.println("");
		}
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
		return new ComboTableModel(qg, scenarioList, regionList, null, null);
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
	private static double getVariableSumValues(String varName, String year, Region region) {
		if(region == null) {
			// TODO: log this?
			return 0.0;
		}

		Wrapper[] workingVar = region.getWorkingM(varName, year);
		if(workingVar == null) {
			// TODO: log this?
			return 0.0;
		}

		Wrapper[] ret = ComponentManipulator.sumValues(workingVar);
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
}
