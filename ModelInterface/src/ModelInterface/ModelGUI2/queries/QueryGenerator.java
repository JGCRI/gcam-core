package ModelInterface.ModelGUI2.queries;

import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.XMLDB;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.undo.EditQueryUndoableEdit;
import ModelInterface.ModelGUI2.undo.MiUndoableEditListener;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.awt.Frame;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.EventListener;

import javax.swing.*;
import java.awt.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import java.util.regex.*;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class QueryGenerator implements java.io.Serializable{
	private transient Frame parentFrame;
	String xPath;
	String var;
	String nodeLevel;
	String yearLevel;
	boolean sumAll;
	boolean group;
	boolean isSumable;
	String title;
	Object[] levelValues;
	public static Vector sumableList;
	public static java.util.List<String> hasYearList;
	String axis1Name;
	String axis2Name;
	private transient QueryBuilder qb;
	int currSel;
	String labelColumnName;
	String comments;
	public QueryGenerator(Frame parentFrameIn) {
		qb = null;
		isSumable = false;
		xPath = "";
		parentFrame = parentFrameIn;
		sumAll = false;
		getQueryDialog();
	}
	public QueryGenerator(Node queryIn) {
		if(queryIn.getNodeName().equals(MarketQueryBuilder.xmlName)) {
			qb = new MarketQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(SupplyDemandQueryBuilder.xmlName)) {
			qb = new SupplyDemandQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(DemographicsQueryBuilder.xmlName)) {
			qb = new DemographicsQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(ResourceQueryBuilder.xmlName)) {
			qb = new ResourceQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(EmissionsQueryBuilder.xmlName)) {
			qb = new EmissionsQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(CostCurveQueryBuilder.xmlName)) {
			qb = new CostCurveQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(GDPQueryBuilder.xmlName)) {
			qb = new GDPQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(ClimateQueryBuilder.xmlName)) {
			qb = new ClimateQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(LandAllocatorQueryBuilder.xmlName)) {
			qb = new LandAllocatorQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(InputQueryBuilder.xmlName)) {
			qb = new InputQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(InputOutputQueryBuilder.xmlName)) {
			qb = new InputOutputQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(DemandComponentsQueryBuilder.xmlName)) {
			qb = new DemandComponentsQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(SAMQueryBuilder.xmlName)) {
			qb = new SAMQueryBuilder(this);
			/* TODO: I lost these, need to re-write
		} else if(queryIn.getNodeName().equals(SectorReportQueryBuilder.xmlName)) {
			qb = new SectorReportQueryBuilder(this);
		} else if(queryIn.getNodeName().equals(GovtResultsQueryBuilder.xmlName)) {
			qb = new GovtResultsQueryBuilder(this);
			*/
		} else {
			qb = null;
		}
		title = ((Element)queryIn).getAttribute("title");
		if(qb == null) {
			System.out.println("Didn't find builder for "+title+" query going to use defaults");
		}
		NodeList nl = queryIn.getChildNodes();
		for(int i = 0; i < nl.getLength(); ++i) {
			if(nl.item(i).getNodeName().equals("axis1")) {
				nodeLevel = nl.item(i).getFirstChild().getNodeValue();
				axis1Name = ((Element)nl.item(i)).getAttribute("name");
			} else if(nl.item(i).getNodeName().equals("axis2")) {
				yearLevel = nl.item(i).getFirstChild().getNodeValue();
				axis2Name = ((Element)nl.item(i)).getAttribute("name");
			} else if(nl.item(i).getNodeName().equals("chartLabelColumn")) {
				labelColumnName = nl.item(i).getFirstChild().getNodeValue();
			} else if(nl.item(i).getNodeName().equals("comments")) {
				Node cmtNodeTemp = nl.item(i).getFirstChild();
				if(cmtNodeTemp != null) {
					comments = cmtNodeTemp.getNodeValue();
				}
			} else if (nl.item(i).getNodeName().equals("xPath")) {
				var = ((Element)nl.item(i)).getAttribute("dataName");
				if( ((Element)nl.item(i)).getAttribute("sumAll").equals("true")) {
					sumAll = true;
				} else {
					sumAll = false;
				}
				if( ((Element)nl.item(i)).getAttribute("group").equals("true")) {
					group = true;
				} else {
					group = false;
				}
				xPath = nl.item(i).getFirstChild().getNodeValue();
				if(((!sumAll && !group) || (sumAll && group)) && !(qb instanceof CostCurveQueryBuilder)
						&& !(qb instanceof GDPQueryBuilder)) {
					Vector temp = new Vector();
					String xpTemp = xPath;
					Pattern pat;
					if(qb instanceof DemographicsQueryBuilder) {
						//System.out.println("HERE");
						pat = Pattern.compile("\\(@ageGroup='([\\d\\-+[\\s]]+)'\\)");
					} else if(qb instanceof EmissionsQueryBuilder) {
						pat = Pattern.compile("\\(@fuel-name='([\\w:[\\s]]+)'\\)");
					} else {
						pat = Pattern.compile("\\(@name='([\\w:[\\s]]+)'\\)");
					}
					Matcher mat = pat.matcher(xPath);
					//DbViewer.xmlDB.setQueryFunction("distinct-values(");
					//DbViewer.xmlDB.setQueryFilter("/scenario/world/region/");
					int skip = 0;
					if(nodeLevel.equals("sector")) {
						currSel = 3;
					} else if(nodeLevel.equals("subsector")) {
						currSel = 4;
						skip = 1;
					} else if(nodeLevel.equals("technology")){
						currSel = 5;
						skip = 2;
					} else if(nodeLevel.equals("grade")) {
						skip = 2;
					} else if(nodeLevel.equals("input")) {
						currSel = 6;
						skip = 3;
					}
					while( mat.find()) {
						if(mat.group(1).startsWith("Group:")) {
							xpTemp = xpTemp.replace("(@name='"+mat.group(1)+"')", expandGroupName(mat.group(1).substring(7)));
						}
						if(skip == 0) {
							temp.add(mat.group(1));
						} else {
							--skip;
						}
					} 
					//DbViewer.xmlDB.setQueryFunction("");
					//DbViewer.xmlDB.setQueryFilter("");
					xPath = xpTemp;
					levelValues = temp.toArray();
				} else {
					levelValues = null;
				}
			}
		}
	}
	protected void getQueryDialog() {
		final QueryGenerator thisGen = this;
		final JDialog filterDialog = new JDialog(parentFrame, "Create Query", true);
		filterDialog.setSize(500,400);
		filterDialog.setLocation(100,100);
		filterDialog.setResizable(false);

		final Map typeMap = new LinkedHashMap();
		typeMap.put("SupplyDemand", new Boolean(false));
		typeMap.put("Market", new Boolean(false));
		typeMap.put("Demographics", new Boolean(false));
		typeMap.put("Resource", new Boolean(false));
		typeMap.put("Emissions", new Boolean(false));
		typeMap.put("Cost Curves", new Boolean(false));
		typeMap.put("GDP", new Boolean(false));
		typeMap.put("Climate", new Boolean(false));
		typeMap.put("LandAllocator", new Boolean(false));
		// TODO: typeMap.put("SGM Queries", new Boolean(false));
		/*
		typeMap.put("Input", new Boolean(false));
		typeMap.put("Demand Components Table", new Boolean(false));
		typeMap.put("Social Accounting Matrix", new Boolean(false));
		typeMap.put("Input Output Table", new Boolean(false));
		typeMap.put("Sector Report Table", new Boolean(false));
		typeMap.put("Government Results Table", new Boolean(false));
		*/
		typeMap.put("Query Group", new Boolean(false));
		final Vector types = new Vector(typeMap.keySet().size(), 0);
		for(int i = 0; i < types.capacity(); ++i) {
			types.add(null);
		}

		final JComponentAdapter[] list = new JComponentAdapter[1];
		list[0] = new JListAdapter(new JList());
		final JScrollPane listScroll = new JScrollPane(list[0].getModel());
		((JList)list[0].getModel()).setCellRenderer(new ListCellRenderer() {
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
				boolean cellHasFocus) {
				Component comp = (new DefaultListCellRenderer()).getListCellRendererComponent(list, value, 
					index, isSelected, cellHasFocus);
				if(((String)value).startsWith("Group")) {
					comp.setBackground(isSelected? Color.BLUE : Color.GREEN );
				}
				return comp;
			}
		});
		final JLabel listLabel = new JLabel();
		listLabel.setHorizontalAlignment(JLabel.LEFT);
		currSel = 0;

		final String cancelTitle = " Cancel ";

		final JButton cancelButton = new JButton(cancelTitle);
		final JButton backButton = new JButton(" < Back ");
		final JButton nextButton = new JButton(" Next > ");

		final Container filterContent = filterDialog.getContentPane();

		final JPanel buttonPane = new JPanel();
		final JPanel listPane = new JPanel();
		final JPanel inputPane = new JPanel();

		final JTextField titleField = new JTextField();
		titleField.setPreferredSize(new Dimension(30, 12));

		final EventListener[] currListener = new EventListener[1];
		currListener[0] = null;
		final ListSelectionListener typeListener = (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if(currSel != 1) {
					return;
				}
				if(list[0].getSelectedRows().length > 0) {
					if(list[0].getSelectedRows()[0] == typeMap.keySet().size()-1) {
						nextButton.setEnabled(false);
						cancelButton.setText("Finished");
					} else {
						nextButton.setEnabled(true);
						cancelButton.setText(cancelTitle);
					}
				} else {
					nextButton.setEnabled(false);
				}
			}
		});
		list[0].addSelectionListener(typeListener);

		backButton.setMnemonic(KeyEvent.VK_B);
		nextButton.setMnemonic(KeyEvent.VK_N);
	        cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!cancelButton.getText().equals(cancelTitle)) {
					if(qb != null) {
						qb.doFinish(list[0]);
					} else {
						xPath = "Query Group";
					}
				}
				//exit this dialog..
				filterDialog.dispose();
				//filterDialog.hide();
			}
		});

            // when we go back, if we can, get the previous nodeName, and load up all its attributes
	    // make the cancel buttons title cancel again if it was on finished
	    backButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				currSel--;
				if(currSel == 1) {
					list[0].removeSelectionListener(currListener[0]);
					list[0].addSelectionListener(typeListener);
					qb = null;
					updateList(typeMap, list[0], listLabel);
				} else if(currSel != 0) {
					JComponentAdapter tempComp = qb.doBack(list[0], listLabel);
					if(tempComp != list[0]) {
						list[0].removeSelectionListener(currListener[0]);
						list[0] = tempComp;
						listScroll.getViewport().setView(list[0].getModel());
						currListener[0] = qb.getListSelectionListener(list[0], nextButton, cancelButton);
						list[0].addSelectionListener(currListener[0]);
					}
				} else {
					filterContent.remove(listPane);
					filterContent.add(inputPane);
					filterDialog.repaint();
				}
				if (!nextButton.isEnabled()) {
					nextButton.setEnabled(true);
					cancelButton.setText(cancelTitle);
				}
				if (currSel == 0) {
					backButton.setEnabled(false);
				}
			}
		});

            // when we go next, if we can, get the next nodeName, and load up all its attributes
	    // make the cancel buttons title finished if we have reached the end of the list of nodeNames 
	    nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				currSel++;
				nextButton.setEnabled(false);
				if(currSel == 1) {
					title = titleField.getText();
					filterContent.remove(inputPane);
					filterContent.add(listPane);
					updateList(typeMap, list[0], listLabel);
				} else if(currSel == 2) {
					Object[] selectedKeys = list[0].getSelectedValues();
					for(Iterator it = typeMap.entrySet().iterator(); it.hasNext(); ) {
						((Map.Entry)it.next()).setValue(new Boolean(false));
					}
					for(int i = 0; i < selectedKeys.length; ++i) {
						typeMap.put(selectedKeys[i], new Boolean(true));
					}
					list[0].removeSelectionListener(typeListener);
					int selInd = list[0].getSelectedRows()[0];
					if(types.get(selInd) == null) {
						switch(selInd) {
							case 0: {
									types.set(selInd, new SupplyDemandQueryBuilder(thisGen));
									break;
							}
							case 1: {
									types.set(selInd, new MarketQueryBuilder(thisGen));
									break;
							}
							case 2: {
									types.set(selInd, new DemographicsQueryBuilder(thisGen));
									break;
							}
							case 3: {
									types.set(selInd, new ResourceQueryBuilder(thisGen));
									break;
							}
							case 4: {
									types.set(selInd, new EmissionsQueryBuilder(thisGen));
									break;
							}
							case 5: {
									types.set(selInd, new CostCurveQueryBuilder(thisGen));
									break;
							}
							case 6: {
									types.set(selInd, new GDPQueryBuilder(thisGen));
									break;
							}
							case 7: {
									types.set(selInd, new ClimateQueryBuilder(thisGen));
									break;
							}
							case 8: {
									types.set(selInd, new LandAllocatorQueryBuilder(thisGen));
									break;
							}
							/* TODO: Again lost, rewrite
							case 9: {
									types.set(selInd, new SGMQueryBuilder(thisGen));
									break;
							}
							*/
							/*
							case 10: {
									types.set(selInd, new InputQueryBuilder(thisGen));
									break;
							}
							case 10: {
									types.set(selInd, new DemandComponentsQueryBuilder(thisGen));
									break;
							}
							case 11: {
									types.set(selInd, new SAMQueryBuilder(thisGen));
									break;
							}
							case 12: {
									types.set(selInd, new InputOutputQueryBuilder(thisGen));
									break;
							}
							case 13: {
									types.set(selInd, new SectorReportQueryBuilder(thisGen));
									break;
							}
							case 14: {
									types.set(selInd, new GovtResultsQueryBuilder(thisGen));
									break;
							}
							*/
							default: {
									System.out.println("Couldn't make type, index: "+selInd);
							}
						}
					}
					qb = (QueryBuilder)types.get(selInd);
					JComponentAdapter tempComp = qb.doNext(list[0], listLabel);
					if(tempComp != list[0]) {
						list[0] = tempComp;
						listScroll.getViewport().setView(list[0].getModel());
					}
					currListener[0] = qb.getListSelectionListener(list[0], nextButton, cancelButton);
					list[0].addSelectionListener(currListener[0]);
				} else {
					JComponentAdapter tempComp = qb.doNext(list[0], listLabel);
					if(tempComp != list[0]) {
						list[0].removeSelectionListener(currListener[0]);
						list[0] = tempComp;
						listScroll.getViewport().setView(list[0].getModel());
						currListener[0] = qb.getListSelectionListener(list[0], nextButton, cancelButton);
						list[0].addSelectionListener(currListener[0]);
					}
				}
				if (!backButton.isEnabled()) {
					backButton.setEnabled(true);
				}
				/*
				if (qb != null && qb.isAtEnd()) {
					nextButton.setEnabled(false);
					//cancelButton.setText("Finished");
				}
				*/
			}
		});

	        // set display properties
		//JPanel buttonPane = new JPanel();
	    	buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
	    	buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	    	buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(backButton);
		backButton.setEnabled(false);

	    	buttonPane.add(nextButton);
	    	buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
	    	buttonPane.add(cancelButton);

		//JPanel listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		listPane.add(Box.createVerticalGlue());
		listPane.add(listLabel);
		listPane.add(Box.createVerticalStrut(10));
		listScroll.setPreferredSize(new Dimension(150, 750));
		listPane.add(listScroll);
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(new JSeparator(SwingConstants.HORIZONTAL));


		inputPane.setLayout( new BoxLayout(inputPane, BoxLayout.Y_AXIS));
		inputPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		JLabel titleLabel = new JLabel("Please enter a title for the query");
		inputPane.add(titleLabel);
		inputPane.add(Box.createVerticalStrut(10));
		inputPane.add(titleField);
	    	inputPane.add(Box.createVerticalGlue());
		inputPane.add(Box.createVerticalStrut(260));
		inputPane.add(new JSeparator(SwingConstants.HORIZONTAL));

		filterContent.add(inputPane, BorderLayout.CENTER);
		filterContent.add(buttonPane, BorderLayout.PAGE_END);

		filterDialog.setContentPane(filterContent);
		filterDialog.setVisible(true);
	}
	private void updateList(Map typeMap, JComponentAdapter list, JLabel listLabel) {
		listLabel.setText("Select type: ");
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		Vector tempVector = new Vector();
		String[] currKeys = (String[])typeMap.keySet().toArray(new String[0]);
		((JList)list.getModel()).setListData(currKeys);
		// check the maps to see which ones are true and add it to the list of selected
		for (int i = 0; i < currKeys.length; ++i) {
			if (((Boolean)typeMap.get(currKeys[i])).booleanValue()) {
				tempVector.addElement(new Integer(i));
			}
		}
		int[] selected = new int[tempVector.size()];
		for (int i = 0; i < selected.length; i++) {
			selected[i] = ((Integer)tempVector.get(i)).intValue();
		}
		tempVector = null;
		list.setSelectedRows(selected);
	}
	private String expandGroupName(String gName) {
		String query;
		StringBuffer ret = new StringBuffer();
		if(currSel == 3) {
			query = "supplysector";
		} else if(currSel == 4) {
			query = "supplysector/subsector";
		} else {
			query = "supplysector/subsector/technology";
		}
		Vector funcTemp = new Vector<String>(1,0);
		funcTemp.add("distinct-values");
		XmlResults res = DbViewer.xmlDB.createQuery(query+"[child::group[@name='"+gName+"']]/@name", 
				"/scenario/world/region", funcTemp);
		funcTemp = null;
		try {
			while(res.hasNext()) {
				ret.append("(@name='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		DbViewer.xmlDB.printLockStats("expandGroupName");
		return ret.toString();
	}
	public String getXPath() {
		return xPath;
	}
	public void setXPath(String xp) {
		xPath = xp;
	}
	public String getVariable() {
		return var;
	}
	public void setVariable(String varIn) {
		var = varIn;
	}
	public String getNodeLevel() {
		return nodeLevel;
	}
	public void setNodeLevel(String nodeLevelIn) {
		nodeLevel = nodeLevelIn;
	}
	public String getAxis1Name() {
		return axis1Name;
	}
	public void setAxis1Name(String a1) {
		axis1Name = a1;
	}
	public String getAxis2Name() {
		return axis2Name;
	}
	public void setAxis2Name(String a2) {
		axis2Name = a2;
	}
	public boolean isSumAll() {
		return sumAll;
	}
	public void setSumAll(boolean s) {
		sumAll = s;
	}
	public boolean isGroup() {
		return group;
	}
	public void setGroup(boolean g) {
		group = g;
	}
	public Node getAsNode(Document doc) {
		Element queryNode; 
		if(qb == null) {
			queryNode = doc.createElement("query");
		} else {
			queryNode = doc.createElement(qb.getXMLName());
		}
		Element temp;
		queryNode.setAttribute("title", title);
		temp = doc.createElement("axis1");
		temp.setAttribute("name", axis1Name);
		temp.appendChild(doc.createTextNode(nodeLevel));
		queryNode.appendChild(temp);
		temp = doc.createElement("axis2");
		temp.setAttribute("name", axis2Name);
		temp.appendChild(doc.createTextNode(yearLevel));
		queryNode.appendChild(temp);
		if(labelColumnName != null && !labelColumnName.equals("")) {
			temp = doc.createElement("chartLabelColumn");
			temp.appendChild(doc.createTextNode(labelColumnName));
			queryNode.appendChild(temp);
		}
		temp = doc.createElement("xPath");
		temp.setAttribute("dataName", var);
		if(sumAll) {
			temp.setAttribute("sumAll", "true");
		} else {
			temp.setAttribute("sumAll", "false");
		}
		if(group) {
			temp.setAttribute("group", "true");
		} else {
			temp.setAttribute("group", "false");
		}
		if(sumAll && group) {
			// do something else
			temp.appendChild(doc.createTextNode(qb.createListPath(-1)));
		} else {
			temp.appendChild(doc.createTextNode(xPath));
		}
		queryNode.appendChild(temp);
		temp = doc.createElement("comments");
		temp.appendChild(doc.createTextNode(comments));
		queryNode.appendChild(temp);
		return queryNode;
	}
	/**
	 * Used to determine if this query has <i>enough</i> information to
	 * be a valid query.  I.E if it was passed an invalid Node into the constructor.
	 * @return True if it is determined enough data had been gathered, false otherwise.
	 */
	public boolean isValid() {
		// checking if qb is null may or may not be a good way to determine if this
		// is valid..
		return title != null && yearLevel != null && nodeLevel != null && var != null &&
			axis1Name != null && axis2Name != null;
	}
	public String toString() {
		return title;
	}
	public void setTitle(String titleIn) {
		title = titleIn;
	}
	public Object[] getLevelValues() {
		return levelValues;
	}
	/**
	 * Returns the comments. If the comments are null
	 * it will default to the title.
	 * @return The comments.
	 * @see getRealComments()
	 */
	public String getComments() {
		return comments != null ? comments : "<i>None</i>";
	}
	/**
	 * Returns the comments no foolin around like
	 * getComments.
	 * @return The comments.
	 * @see getComments()
	 */
	public String getRealComments() {
		return comments;
	}
	public void setComments(String commentsIn) {
		comments = commentsIn;
	}
	public String getYearLevel() {
		return yearLevel;
	}
	public void setYearLevel(String yL) {
		yearLevel = yL;
	}
	public String getChartLabelColumnName() {
		return labelColumnName;
	}
	public void setCharLabelColumnName(String name) {
		labelColumnName = name;
	}
	public String getCompleteXPath(Object[] regions) {
		if(qb != null) {
			return qb.getCompleteXPath(regions);
		} else {
			return defaultCompleteXPath(regions);
		}
	}
  	public Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		if(qb != null) {
			return qb.extractAxisInfo(n, filterMaps);
		} else {
			return defaultAxisInfo(n, filterMaps);
		}
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if(qb != null) {
			return qb.addToDataTree(currNode, dataTree);
		} else {
			return defaultAddToDataTree(currNode, dataTree);
		}
	}
	protected boolean isGlobal;
	protected String defaultCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuffer ret = new StringBuffer();
		if(((String)regions[0]).equals("Global")) {
			ret.append("region/");
			//regionSel = new int[0]; 
			regions = new Object[0];
			isGlobal = true;
		} else {
			isGlobal = false;
		}
		for(int i = 0; i < regions.length; ++i) {
			if(!added) {
				ret.append("region[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
		}
		if(added) {
			ret.append(" ]/");
		}
		return ret.append(xPath).toString();
	}
	protected Object[] defaultAxisInfo(XmlValue n, Map filterMaps) throws Exception {
		Vector ret = new Vector(2,0);
		XmlValue nBefore;
		do {
			if(n.getNodeName().equals(nodeLevel)) {
				ret.add(XMLDB.getAttr(n));
			} 
			if(n.getNodeName().equals(yearLevel)) {
				ret.add(0, XMLDB.getAttr(n, "year"));
			} else if(XMLDB.hasAttr(n)) {
				HashMap tempFilter;
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
		DbViewer.xmlDB.printLockStats("getRegionAndYearFromNode");
		return ret.toArray();
	}
	protected Map defaultAddToDataTree(XmlValue currNode, Map dataTree) throws Exception {
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			currNode.delete();
			return dataTree;
		}
		Map tempMap = defaultAddToDataTree(currNode.getParentNode(), dataTree);
		if( (isGlobal && currNode.getNodeName().equals("region")) || 
				(currNode.getNodeName().matches(".*sector") || currNode.getNodeName().equals("technology"))) {
			currNode.delete();
			return tempMap;
		}
		if(XMLDB.hasAttr(currNode) && !currNode.getNodeName().equals(nodeLevel) && !currNode.getNodeName().equals(yearLevel)) {
			String attr = XMLDB.getAllAttr(currNode);
			attr = currNode.getNodeName()+"@"+attr;
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new TreeMap());
			}
			currNode.delete();
			return (TreeMap)tempMap.get(attr);
		} 
		currNode.delete();
		return tempMap;
	}
	public boolean editDialog(final MiUndoableEditListener listener) {
		final Object lock = new Object();
		String oldTitle = title;
		final JDialog editDialog = new JDialog(InterfaceMain.getInstance(), "Edit Query", false);
		final QueryGenerator thisGen = this;
		//editDialog.setLocation(100,100);
		editDialog.setResizable(false);
		JButton cancelButton = new JButton("Cancel");
		JButton okButton = new JButton("  OK  ");
		JPanel all = new JPanel();
		JPanel tempPanel;
		final JTextField titleTextF = new JTextField(title, 30);
		final JTextField a1NameTextF = new JTextField(axis1Name, 30);
		final JTextField a1TextF = new JTextField(nodeLevel, 20);
		final JTextField a2NameTextF = new JTextField(axis2Name, 30);
		final JTextField a2TextF = new JTextField(yearLevel, 20);
		final JTextField dataNameTextF = new JTextField(var, 30);
		final JTextField xPathTextF = new JTextField(xPath, 50);
		final JCheckBox sumAllCheckBox = new JCheckBox("Sum All", sumAll);
		final JCheckBox groupCheckBox = new JCheckBox("Group", group);
		final JTextField labelCol = new JTextField(labelColumnName, 30);
		final JTextArea commentsTextA = new JTextArea(comments, 4, 30);
		Component seperator = Box.createRigidArea(new Dimension(20, 10));
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		all.add(new JLabel("Edit Query"));

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Title: "));
		tempPanel.add(seperator);
		tempPanel.add(titleTextF);
		tempPanel.add(seperator);
		tempPanel.add(sumAllCheckBox);
		tempPanel.add(seperator);
		tempPanel.add(groupCheckBox);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Axis 1 Name: "));
		tempPanel.add(seperator);
		tempPanel.add(a1NameTextF);
		tempPanel.add(seperator);
		tempPanel.add(new JLabel("  Axis 1 Node: "));
		tempPanel.add(seperator);
		tempPanel.add(a1TextF);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Axis 2 Name: "));
		tempPanel.add(seperator);
		tempPanel.add(a2NameTextF);
		tempPanel.add(seperator);
		tempPanel.add(new JLabel("  Axis 2 Node: "));
		tempPanel.add(seperator);
		tempPanel.add(a2TextF);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Data Name: "));
		tempPanel.add(seperator);
		tempPanel.add(dataNameTextF);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Chart Label Column: "));
		tempPanel.add(seperator);
		tempPanel.add(labelCol);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("XPATH: "));
		tempPanel.add(seperator);
		tempPanel.add(xPathTextF);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Comments: "));
		tempPanel.add(seperator);
		tempPanel.add(commentsTextA);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(okButton);
		tempPanel.add(seperator);
		tempPanel.add(cancelButton);
		all.add(tempPanel);

		final boolean[] didChange = new boolean[1];
		didChange[0] = false;

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				EditQueryUndoableEdit eqEdit = new EditQueryUndoableEdit(thisGen, listener);
				eqEdit.setOldValues(thisGen);
				title = titleTextF.getText();
				axis1Name = a1NameTextF.getText();
				nodeLevel = a1TextF.getText();
				axis2Name = a2NameTextF.getText();
				yearLevel = a2TextF.getText();
				var = dataNameTextF.getText();
				labelColumnName = labelCol.getText();
				xPath = xPathTextF.getText();
				sumAll = sumAllCheckBox.isSelected();
				group = groupCheckBox.isSelected();
				comments = commentsTextA.getText();
				eqEdit.setNewValues(thisGen);
				if(eqEdit.hasRealChanges()) {
					InterfaceMain.getInstance().getUndoManager().addEdit(eqEdit);
					InterfaceMain.getInstance().refreshUndoRedo();
					didChange[0] = true;
				} else {
					// not needed
					eqEdit.die();
				}
				synchronized(lock) {
					lock.notifyAll();
				}
				editDialog.dispose();
			}
		});
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// don't have to update didChange
				synchronized(lock) {
					lock.notifyAll();
				}
				editDialog.dispose();
			}
		});

		editDialog.getContentPane().add(all);
		editDialog.pack();
		editDialog.setVisible(true);
		try {
			synchronized(lock) {
				lock.wait();
			}
		} catch(InterruptedException ie) {
			ie.printStackTrace();
		}
		return didChange[0];
	}
}
