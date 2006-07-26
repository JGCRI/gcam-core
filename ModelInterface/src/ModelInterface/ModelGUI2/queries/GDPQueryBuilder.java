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
import java.util.TreeMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class GDPQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "gdpQueryBuilder";
	public GDPQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		return updateList(list, label);
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
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
		//qg.levelValues = list.getSelectedValues();
		qg.levelValues = null;
		queryFunctions = null;
		queryFilter = null;
	}
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		System.out.println("Would I do anything here");
		return list;
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					temp = varList;
					label.setText("Select Cost Type:");
					break;
			}
			/*
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(sectorList == null) {
						sectorList = createList("supplysector/@name", false);
						sectorList.putAll(createList("supplysector/group/@name", true));
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
			*/
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
		return list;
	}
	public void updateSelected(JComponentAdapter list) {
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
			/*
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
			*/
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		for(Iterator it = selected.entrySet().iterator(); it.hasNext(); ) {
			((Map.Entry)it.next()).setValue(new Boolean(false));
		}
		for(int i = 0; i < selectedKeys.length; ++i) {
			selected.put(selectedKeys[i], new Boolean(true));
		}
	}
	private void createXPath() {
		String typeSel = null;
		for(Iterator i = varList.entrySet().iterator(); i.hasNext(); ) {
			Map.Entry me = (Map.Entry)i.next();
			if(((Boolean)me.getValue()).booleanValue()) {
				typeSel = (String)me.getKey();
			}
		}
		qg.xPath = "GDP/"+typeSel+"/text()";
		qg.nodeLevel = "region";
		qg.yearLevel = typeSel;
		qg.axis1Name = "Region";
		qg.axis2Name = "Year";
		qg.var = typeSel;
		qg.sumAll = false;
		qg.group = false;
	}
	boolean isGlobal;
	public String getCompleteXPath(Object[] regions) {
		System.out.println("Trying to complete xpath");
		StringBuffer strBuff = new StringBuffer();
		strBuff.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1));
		if(((String)regions[0]).equals("Global")) {
			strBuff.append("]/").append(qg.xPath);
			isGlobal = true;
			return strBuff.toString();
		} else {
			isGlobal = false;
			strBuff.append(" and (");
			for(int i = 0; i < regions.length; ++i) {
				strBuff.append(" (@name='").append(regions[i]).append("') or ");
			}
			strBuff.replace(strBuff.length()-3, strBuff.length(), ")]/");
			strBuff.append(qg.xPath);
			return strBuff.toString();
		}
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2, 0);
		XmlValue nBefore;
		do {
			if(qg.nodeLevel.equals(XMLDB.getAttr(n, "type"))) {
				if(!isGlobal) {
					ret.add(XMLDB.getAttr(n, "name"));
				} else {
					ret.add("Global");
				}
			} 
			if(n.getNodeName().equals(qg.yearLevel)) {
				ret.add(0, XMLDB.getAttr(n, "year"));
			} else if(XMLDB.hasAttr(n)) {
				Map tempFilter;
				if (filterMaps.containsKey(n.getNodeName())) {
					tempFilter = (HashMap)filterMaps.get(n.getNodeName());
				} else {
					tempFilter = new HashMap();
				}
				String attr = XMLDB.getAttr(n);
				if (!tempFilter.containsKey(attr)) {
					tempFilter.put(attr, new Boolean(true));
					filterMaps.put(n.getNodeName(), tempFilter);
				}
			}
			nBefore = n;
			n = n.getParentNode();
			nBefore.delete();
		} while(n.getNodeType() != XmlValue.DOCUMENT_NODE); 
		n.delete();
		DbViewer.xmlDB.printLockStats("GDPQueryBuilder.getRegionAndYearFromNode");
		return ret.toArray();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		// used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
		/*
		if( (isGlobal && currNode.getNodeName().equals("region")) ) {
			currNode.delete();
			return tempMap;
		}
		*/
		if(XMLDB.hasAttr(currNode) && !qg.nodeLevel.equals(XMLDB.getAttr(currNode, "type"))
				&& !currNode.getNodeName().equals(qg.yearLevel)) {
			String attr = XMLDB.getAllAttr(currNode);
			attr = currNode.getNodeName()+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
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
