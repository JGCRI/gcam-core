package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.XMLDB;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.ListSelectionModel;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class InputOutputQueryBuilder extends QueryBuilder {
	/*
	public static Map varList;
	protected Map sectorList;
	protected Map subsectorList;
	protected Map techList;
	protected Map inputList;
	*/
	private static final String inputQueryPortion = "*[@type = 'input']";
	public static String xmlName = "inputOutputQuery";
	public InputOutputQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		//sectorList = null;
		//subsectorList = null;
		//techList = null;
		//inputList = null;
		// for now..
		//varList = new HashMap();
		//varList.put("demand-currency", false);
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/";
		//DbViewer.xmlDB.setQueryFunction("distinct-values(");
		//DbViewer.xmlDB.setQueryFilter("/scenario/world/region/");
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
					/*
				} else if(qg.currSel == 1 || qg.currSel == 2) {
					nextButton.setEnabled(true);
				} else if((qg.isSumable && (selectedInd[0] == 0 || selectedInd[0] == 1)) || selectedInd.length > 1
					|| ((String)list.getSelectedValues()[0]).startsWith("Group:")) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 6 && !qg.isSumable) {
					nextButton.setEnabled(true);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 6){
					nextButton.setEnabled(true);
					*/
				} else {
					cancelButton.setText("Finished");
				}
			}
		});
	}
	public void doFinish(JComponentAdapter list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		qg.levelValues = list.getSelectedValues();
		queryFunctions = null;
		queryFilter = null;
		//DbViewer.xmlDB.setQueryFunction("");
		//DbViewer.xmlDB.setQueryFilter("");
	}
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		// doing this stuff after currSel has changed now..
		// have to sub 1
		/*
		if(qg.currSel == 2) {
			sectorList = null;
		} else if(qg.currSel == 3) {
			subsectorList = null;
		} else if(qg.currSel == 4) {
			techList = null;
		} else if(qg.currSel == 5) {
			inputList = null;
		}
		*/
		return updateList(list, label);
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		// being moved to after currSel changed, adjust numbers
		updateSelected(list);
		if(qg.currSel == 3) {
			qg.var = "demand-currency";
			qg.isSumable = false;
		}
		return updateList(list, label);
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		/*
		Map temp = new HashMap();
		temp.put("Select This!!", false);
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					label.setText("Select Stuff:");
					break;
			}
			*/
			/*
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(sectorList == null) {
						sectorList = createList(sectorQueryPortion+"/@name", false);
						sectorList.putAll(createList(sectorQueryPortion+"/group/@name", true));
					}
					temp = sectorList;
					label.setText("Select Sector:");
					break;
			}
			case 4: {
					if(subsectorList == null) {
						subsectorList = createList(createListPath(4), false);
					}
					temp = subsectorList;
					label.setText("Select Subsector:");
					break;
			}
			case 5: {
					if(techList == null) {
						techList = createList(createListPath(5), false);
					}
					temp = techList;
					label.setText("Select Technology:");
					break;
			}
			case 6: {
					if(inputList == null) {
						inputList = createList(createListPath(6), false);
					}
					temp = inputList;
					label.setText("Select Input:");
					break;
			}
			*/
		/*
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		Vector tempVector = new Vector();
		String[] currKeys = (String[])temp.keySet().toArray(new String[0]);
		((JList)list.getModel()).setListData(currKeys);
		// check the maps to see which ones are true and add it to the list of selected
		for (int i = 0; i < currKeys.length; ++i) {
			if (((Boolean)temp.get(currKeys[i])).booleanValue()) {
				tempVector.addElement(new Integer(i));
			}
		}
		int[] selected = new int[tempVector.size()];
		for (int i = 0; i < selected.length; i++) {
			selected[i] = ((Integer)tempVector.get(i)).intValue();
		}
		temp = null;
		tempVector = null;
		list.setSelectedRows(selected);
		*/
		return list;
	}
	public void updateSelected(JComponentAdapter list) {
		/*
		Object[] selectedKeys = list.getSelectedValues();
		Map selected = null;
		switch(qg.currSel -1) {
			case 1: {
					return;
			}
			case 2: {
					selected = varList;
					break;
			}
			case 3: {
					selected = sectorList;
					break;
			}
			case 4: {
					selected = subsectorList;
					break;
			}
			case 5: {
					selected = techList;
					break;
			}
			case 6: {
					selected = inputList;
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		for(Iterator it = selected.entrySet().iterator(); it.hasNext(); ) {
			((Map.Entry)it.next()).setValue(new Boolean(false));
		}
		for(int i = 0; i < selectedKeys.length; ++i) {
			selected.put(selectedKeys[i], new Boolean(true));
		}
		*/
	}
	public String createListPath(int level) {
		/*
		Map tempMap;
		StringBuffer ret = new StringBuffer();
		boolean added = false;
		boolean gq = false;
		if(level == -1) {
			gq = true;
			qg.sumAll = qg.group = false;
			level = 7;
		}
		for(int i = 0; i < level-3; ++i) {
			if(i == 0) {
				tempMap = sectorList;
				ret.append(sectorQueryPortion.substring(0, sectorQueryPortion.length()-1));
			} else if(i == 1){
				tempMap = subsectorList;
				ret.append(subsectorQueryPortion.substring(0, subsectorQueryPortion.length()-1));
			} else if(i == 2){
				tempMap = techList;
				ret.append(technologyQueryPortion.substring(0, technologyQueryPortion.length()-1));
				//++i;
			} else {
				tempMap = inputList;
				ret.append(inputQueryPortion.substring(0, inputQueryPortion.length()-1));
				++i;
			}
			added = false;
			if(tempMap == null) {
				ret.append("]/");
				continue;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Sum All")).booleanValue()) {
				qg.sumAll = true;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Group All")).booleanValue()) {
				qg.group = true;
			}
			//for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !((Boolean)tempMap.get("Sum All")).booleanValue(); ) 
			for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !(qg.sumAll || qg.group); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					if(!added) {
						ret.append(" and (");
						added = true;
					} else {
						ret.append(" or ");
					}
					if(!gq && ((String)me.getKey()).startsWith("Group:")) {
						ret.append(expandGroupName(((String)me.getKey()).substring(7)));
						gq = true;
					} else {
						ret.append("(@name='"+me.getKey()+"')");
					}
				}
			}
			if(added) {
				ret.append(" )]/");
			} else {
				ret.append("]/");
			}
		}
		if(level == 3) {
			ret.append(sectorQueryPortion+"/@name");
		} else if(level == 4) {
			ret.append(subsectorQueryPortion+"/@name");
		} else if(level == 5) {
			ret.append(technologyQueryPortion+"/@name");
		} else if(level == 6) {
			ret.append(inputQueryPortion+"/@name");
		} else {
			ret.append(qg.var).append("/node()");
			//ret += "period/"+var+"/node()";
			System.out.println("The xpath is: "+ret.toString());
		}
		if(gq) {
			qg.group = true;
			qg.sumAll = true;
		}
		return ret.toString();
		*/
		// do I need to do anything real?
		return qg.xPath;
	}
	/*
	private String expandGroupName(String gName) {
		String query;
		StringBuffer ret = new StringBuffer();
		if(qg.currSel == 3) {
			//query = "supplysector";
			query = sectorQueryPortion;
		} else if(qg.currSel == 4) {
			query = sectorQueryPortion+"/"+subsectorQueryPortion;
		} else if(qg.currSel == 5){
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion;
		} else {
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion+
				"/"+inputQueryPortion;
		}
		XmlResults res = DbViewer.xmlDB.createQuery(query+"[child::group[@name='"+gName+"']]/@name", queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				ret.append("(@name='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		DbViewer.xmlDB.printLockStats("InputOutputQueryBuilder.expandGroupName");
		return ret.toString();
	}
	*/
	private void createXPath() {
		qg.axis1Name = qg.nodeLevel = "input";
		qg.axis2Name = qg.yearLevel = "sector";
		qg.group = true;
		qg.var = "demand-currency";
		// the query for getting the factor supply names is no good becauase I won't be able to filter to the correct
		// scenarios/regions.  Plus isn't it almost always going to be Land, Labor, and Capital anyways?
		qg.xPath = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+baseTechnologyQueryPortion+"/"
			+inputQueryPortion+"/demand-currency/text() | "+sectorQueryPortion+"/"+subsectorQueryPortion+"/"+
			baseTechnologyQueryPortion+"/*[local-name() = 'output' or local-name() = 'annual-investment']/text() | "+
			"Marketplace/market[child::MarketGoodOrFuel[ child::text() = /scenario/world/regionCGE/factorSupply/@name]]/supply/text()";
	}
	private Map createList(String path, boolean isGroupNames) {
		LinkedHashMap ret = new LinkedHashMap();
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		XmlResults res = DbViewer.xmlDB.createQuery(path, queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				if(!isGroupNames) {
					ret.put(res.next().asString(), new Boolean(false));
				} else { 
					ret.put("Group: "+res.next().asString(), new Boolean(false));
				}
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		res.delete();
		DbViewer.xmlDB.printLockStats("InputOutputQueryBuilder.createList");
		return ret;
	}
	protected boolean isGlobal;
	public String getCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuilder regionQ = new StringBuilder();
		// for somereason you can not append a StringBuilder to
		// a StringBuilder, but you can append a StringBuffer to
		// a StringBuilder.  Whatever..
		StringBuffer marketRegionQ = new StringBuffer();

		if(((String)regions[0]).equals("Global")) {
			regionQ.append(regionQueryPortion+"/");
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length && !isGlobal; ++i) {
			if(!added) {
				regionQ.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1)).append(" and (");
				marketRegionQ.append(" and ContainedRegion[");
				added = true;
			} else {
				regionQ.append(" or ");
				marketRegionQ.append(" or ");
			}
			regionQ.append("(@name='").append(regions[i]).append("')");
			marketRegionQ.append("(child::text() = '").append(regions[i]).append("')");
		}
		String[] queries = qg.getXPath().split("\\s*\\|\\s*");
		// this is sort of hard coding that I know the market query will be last,
		// and that there will only be 1 market query.. think of a better way
		if(added) {
			regionQ.append(" )]/");
			marketRegionQ.append(" ] ]/");
			String[] spStr = queries[2].split("\\]/");
			marketRegionQ.insert(0, spStr[0]).append(spStr[1]).toString();
		} else {
			marketRegionQ.append(queries[2]);
		}
		String regionFilter = regionQ.toString();
		//System.out.println("The 3 parts are : >>"+queries[0]+"<< | >>"+queries[1]+"<< | >>"+queries[2]+"<<");
		return regionQ.append(queries[0]).append(" | ").append(regionFilter).append(queries[1])
			.append(" | ").append(marketRegionQ).toString();
		/*
		int pipeIndex = qg.getXPath().indexOf('|');
		String part1 = qg.getXPath().substring(0, pipeIndex+1);
		String part2 = qg.getXPath().substring(pipeIndex+1, qg.getXPath().length());
		String retStr = ret.toString();
		//System.out.println("XPath would be: "+ret.append(part1).append(retStr).append(part2));
		//return ret.append(qg.getXPath()).toString();
		return ret.append(part1)
			.append(" collection('database.dbxml')/scenario[ (@name='test' and @date='2006-27-9T22:20:46-00:00') ]/world/")
			.append(retStr).append(part2).toString();
			*/
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2, 0);
		boolean isOutput = false;
		boolean isAnualInvestment = false;
		if(n.getNodeName().equals("output")) {
			//System.out.println("Finds output");
			isOutput = true;
			// try to negitiveify n's value?
			double tempVal = n.getFirstChild().asNumber();
			XmlValue changeTo = new XmlValue(-1*tempVal);
			XmlValue.setValue(changeTo, n.getFirstChild());
		} else if(n.getNodeName().equals("annual-investment")) {
			//System.out.println("Finds anual investment");
			isAnualInvestment = true;
		} else if(n.getNodeName().equals("supply")) {
			// TODO: check lock stats here, do I need to do some deleteing?
			XmlValue prevSib = n.getPreviousSibling();
			while(prevSib != null && !prevSib.getNodeName().equals("MarketGoodOrFuel")) {
				prevSib = prevSib.getPreviousSibling();
			}
			if(prevSib == null) {
				prevSib = n.getNextSibling();
				while(prevSib != null && !prevSib.getNodeName().equals("MarketGoodOrFuel")) {
					prevSib = prevSib.getNextSibling();
				}
			}
			// if it is still null something is wrong..
			ret.add("Household");
			ret.add(prevSib.getFirstChild().getNodeValue());
			//System.out.println("Returning ret: "+ret.get(0)+"---"+ret.get(1));
			return ret.toArray();
		}
		XmlValue nBefore;
		boolean isProductionSector = false;
		do {
			if(qg.nodeLevel.equals(XMLDB.getAttr(n, "type"))) {
				ret.add(XMLDB.getAttr(n, "name"));
			} 
			if(qg.yearLevel.equals(XMLDB.getAttr(n, "type"))) {
				ret.add(0, XMLDB.getAttr(n, "name"));
				if(isOutput) {
					//System.out.println("Did set output");
					ret.add(ret.get(0));
				} else if(isAnualInvestment) {
					//System.out.println("Did set invest");
					ret.add("Capital");
				} else if(n.getNodeName().equals("productionSector")) {
					//System.out.println("Might set OVA");
					isProductionSector = true;
				}
			}
			nBefore = n;
			n = n.getParentNode();
			nBefore.delete();
		} while(n.getNodeType() != XmlValue.DOCUMENT_NODE); 
		n.delete();
		DbViewer.xmlDB.printLockStats("InputOutputQueryBuilder.getRegionAndYearFromNode");
		// The capital row is not truely capital but other value added.
		// The row is only the OVA row in ProductionSectors, it behaves as capital in Consumers.
		if(isProductionSector && ret.get(1).equals("Capital")) { 
			//System.out.println("Did set OVA");
			ret.set(1, "OVA");
		}
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		String type = XMLDB.getAttr(currNode, "type");
		if(type == null) {
			type = currNode.getNodeName();
		}
		if(type.equals("demand-currency") || type.equals("region") || type.equals("output") || type.equals("annual-investment") ||
				type.equals("supply") || type.equals("market")) {
			String attr;
			if(type.equals("supply")) {
				// have to do some weirdness here as the structure is not quite right,
				// and year has to get found before the region
				attr = "year@"+XMLDB.getAttr(currNode.getParentNode(), "year");
			} else if(type.equals("market")) {
				XmlValue regionNameNode = currNode.getFirstChild();
				// if we hit null we have problems anyways..
				while(!regionNameNode.getNodeName().equals("MarketRegion")) {
					regionNameNode = regionNameNode.getNextSibling();
				}
				attr = "region@"+regionNameNode.getFirstChild().getNodeValue();
			} else if(type.equals("region")) {
				attr = "region@"+XMLDB.getAttr(currNode, "name");
			} else {
				attr = "year@"+XMLDB.getAttr(currNode, "year");
			}
			//attr = type+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new HashMap());
			}
			currNode.delete();
			return (Map)tempMap.get(attr);
		} 
		currNode.delete();
		return tempMap;
	}
	public String getXMLName() {
		return xmlName;
	}
}
