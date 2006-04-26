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
import java.util.Iterator;
import java.util.Vector;
import java.util.HashMap;
import java.util.TreeMap;

import com.sleepycat.dbxml.XmlValue;

public class ClimateQueryBuilder extends QueryBuilder {
	public static Map varList;
	public static String xmlName = "ClimateQuery";
	public ClimateQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
		varList = new HashMap();
		varList.put("CO2 Concentration", false);
	}
	public String createListPath(int level) {
		System.out.println("This Method doesn't do anything");
		return null;
	}
	public void doNext(JList list, JLabel label) {
		//System.out.println("This Method doesn't do anything");
		updateList(list, label);
	}
	public ListSelectionListener getListSelectionListener(final JList list, final JButton nextButton, final JButton cancelButton) {
		/*
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/";
		*/
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedIndices();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
				} else {
					cancelButton.setText("Finished");
				}
			}
		});
	}
	public void doFinish(JList list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		//qg.levelValues = list.getSelectedValues();
		qg.levelValues = null;
		queryFunctions = null;
		queryFilter = null;
	}
	public void doBack(JList list, JLabel label) {
		System.out.println("Would I do anything here");
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public void updateList(JList list, JLabel label) {
		Map temp = null;
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					temp = varList;
					//list.setListData(varList.keySet().toArray());
					label.setText("Select Cost Type:");
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		Vector tempVector = new Vector();
		String[] currKeys = (String[])temp.keySet().toArray(new String[0]);
		list.setListData(currKeys);
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
		list.setSelectedIndices(selected);
	}
	public void updateSelected(JList list) {
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
		if(typeSel.equals("CO2 Concentration")) {
			qg.xPath = "/climate-model/co2-concentration/text()";
			qg.axis1Name = "Year";
			qg.yearLevel = qg.nodeLevel = "co2-concentration";
			qg.var = qg.axis2Name = "CO2 Concentration";
		}
		qg.group = false;
		qg.sumAll = false;
	}
	protected boolean isGlobal;
	public String getCompleteXPath(Object[] regions) {
		// ignoring selected regions
		return qg.xPath;
	}
	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Object[] ret = new Object[2];
		ret[0] = XMLDB.getAttr(n, "year");
		ret[1] = "CO2 Concentration";
		DbViewer.xmlDB.printLockStats("ClimateQueryBuilder.extractAxisInfo");
		return ret;
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
		if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(qg.yearLevel)) {
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
