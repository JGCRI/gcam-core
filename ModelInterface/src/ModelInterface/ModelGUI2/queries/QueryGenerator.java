package ModelInterface.ModelGUI2.queries;

import ModelInterface.InterfaceMain;
import ModelInterface.common.DataPair;
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
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.EventListener;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import javax.swing.*;
import javax.swing.text.JTextComponent;
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
	boolean sumAll;
	boolean group;
	boolean isSumable;
	boolean buildSingleQueryList;
	String title;
	Object[] levelValues;
	public static Vector sumableList;
	public static java.util.List<String> hasYearList;
	String axis1Name;
	String axis2Name;

	// The level has a pair of a nodename
	// and the attribute name to pull data from
	// if the attribute name is null it will
	// assume the usual.
	DataPair<String, String> nodeLevel;
	DataPair<String, String> yearLevel;

	List<String> collapseOnList;
	private QueryBuilder qb;
	int currSel;
	String labelColumnName;
	String comments;
	private transient SingleQueryExtension singleExtension;

	public QueryGenerator(Frame parentFrameIn) {
		qb = null;
		isSumable = false;
		buildSingleQueryList = true;
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
				nodeLevel = parseAxisLevel(nl.item(i).getFirstChild().getNodeValue());
				axis1Name = ((Element)nl.item(i)).getAttribute("name");
			} else if(nl.item(i).getNodeName().equals("axis2")) {
				yearLevel = parseAxisLevel(nl.item(i).getFirstChild().getNodeValue());
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
				if( ((Element)nl.item(i)).getAttribute("buildList").equals("") || ((Element)nl.item(i)).getAttribute("buildList").equals("true")) {
					buildSingleQueryList = true;
				} else {
					buildSingleQueryList = false;
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
					if(nodeLevel.getKey().equals("sector")) {
						currSel = 3;
					} else if(nodeLevel.getKey().equals("subsector")) {
						currSel = 4;
						skip = 1;
					} else if(nodeLevel.getKey().equals("technology")){
						currSel = 5;
						skip = 2;
					} else if(nodeLevel.getKey().equals("grade")) {
						skip = 2;
					} else if(nodeLevel.getKey().equals("input")) {
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
		final JButton gatherButton = new JButton("Get New Variables");
		gatherButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				DbViewer.xmlDB.addVarMetaData(parentFrame);
			}
		});

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
		buttonPane.add(gatherButton);
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
		XmlResults res = DbViewer.xmlDB.createQuery("/scenario/world/region"+
				query+"[child::group[@name='"+gName+"']]/@name", funcTemp, null, null);
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
	/**
	 * Gets the nodeLevel node name. Without the
	 * attribute name.
	 * @return The nodeLevel node name.
	 */
	public String getNodeLevel() {
		return nodeLevel.getKey();
	}
	public DataPair<String, String> getNodeLevelPair() {
		return nodeLevel;
	}
	/**
	 * Sets the nodeLevel node name.  It does not
	 * parse the attribute name here.
	 * @param nodeLevelIn A node name.
	 */ 
	public void setNodeLevel(String nodeLevelIn) {
		// should I look for an @ sign to make sure
		// no attribute name was passed as well?
		nodeLevel.setKey(nodeLevelIn);
	}
	public void setNodeLevel(DataPair<String, String> nodeLevelIn) {
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
	public boolean isBuildList() {
		return buildSingleQueryList;
	}
	public void setBuildList(boolean list) {
		if(singleExtension != null) {
			singleExtension.setEnabled(list);
		}
		buildSingleQueryList = list;
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
		temp.appendChild(doc.createTextNode(displayAxisLevel(nodeLevel)));
		queryNode.appendChild(temp);
		temp = doc.createElement("axis2");
		temp.setAttribute("name", axis2Name);
		temp.appendChild(doc.createTextNode(displayAxisLevel(yearLevel)));
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
		if(buildSingleQueryList) {
			temp.setAttribute("buildList", "true");
		} else {
			temp.setAttribute("buildList", "false");
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
		return comments != null && !comments.equals("") ? comments : "<i>None</i>";
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
	/**
	 * Get the yearLevel node name. Without the
	 * attribute name.
	 * @return The yearLevel node name.
	 */ 
	public String getYearLevel() {
		return yearLevel.getKey();
	}
	public DataPair<String, String> getYearLevelPair() {
		return yearLevel;
	}
	/**
	 * Sets the yearLevel node name. Does not 
	 * parse the attribute name here.
	 * @param yL A node name.
	 */ 
	public void setYearLevel(String yL) {
		// check for @ to make sure there
		// is not attribute name?
		yearLevel.setKey(yL);
	}
	public void setYearLevel(DataPair<String, String>  yL) {
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
		if(nodeLevel.getKey().equals("keyword")) {
			nBefore = n.getNextSibling();
			while(!nBefore.isNull()) {
				if(nBefore.getNodeName().equals(nodeLevel.getKey()) &&
					XMLDB.getAttr(nBefore, nodeLevel.getValue()) != null) {
					ret.add(XMLDB.getAttr(nBefore, nodeLevel.getValue()));
					break;
				}
				nBefore = nBefore.getNextSibling();
			}
		}
		do {
			if(n.getNodeName().equals(nodeLevel.getKey())) {
				if(nodeLevel.getValue() == null) {
					ret.add(XMLDB.getAttr(n));
				} else {
					ret.add(XMLDB.getAttr(n, nodeLevel.getValue()));
				}
			} 
			if(n.getNodeName().equals(yearLevel.getKey())) {
				if(yearLevel.getValue() == null) {
					ret.add(0, XMLDB.getAttr(n, "year"));
				} else {
					ret.add(0, XMLDB.getAttr(n, yearLevel.getValue()));
				}
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
			List<String> defaultCollapse = new Vector<String>();
			createCollapseList(defaultCollapse);
			currNode.delete();
			return dataTree;
		}
		Map tempMap = defaultAddToDataTree(currNode.getParentNode(), dataTree);
		String type = XMLDB.getAttr(currNode, "type");
		if(type == null) {
			type = currNode.getNodeName();
		}
		// used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
		if( (isGlobal && type.equals("region")) || collapseOnList.contains(type)) {
			currNode.delete();
			return tempMap;
		}
		if(XMLDB.hasAttr(currNode) && !type.equals(nodeLevel.getKey()) && 
				!type.equals(yearLevel.getKey())) {
			String attr = XMLDB.getAllAttr(currNode);
			attr = type+"@"+attr;
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
		final JTextField a1NameTextF = new JTextField(axis1Name, 15);
		final JTextField a1TextF = new JTextField(displayAxisLevel(nodeLevel), 20);
		final JTextField a2NameTextF = new JTextField(axis2Name, 15);
		final JTextField a2TextF = new JTextField(displayAxisLevel(yearLevel), 20);
		final JTextField dataNameTextF = new JTextField(var, 20);
		final JTextComponent xPathTextF = xPath.contains("\n") ? new JTextArea(xPath, 6, 50) : new JTextField(xPath, 50);
		final JCheckBox sumAllCheckBox = new JCheckBox("Sum All", sumAll);
		final JCheckBox groupCheckBox = new JCheckBox("Group", group);
		final JCheckBox buildListCheckBox = new JCheckBox("Build List", buildSingleQueryList);
		final JTextField labelCol = new JTextField(labelColumnName, 15);
		final JTextArea commentsTextA = new JTextArea(comments, 4, 45);

		// set all of the text field's max size to be it's preferred size
		titleTextF.setMaximumSize(titleTextF.getPreferredSize());
		a1NameTextF.setMaximumSize(a1NameTextF.getPreferredSize());
		a1TextF.setMaximumSize(a1TextF.getPreferredSize());
		a2NameTextF.setMaximumSize(a2NameTextF.getPreferredSize());
		a2TextF.setMaximumSize(a2TextF.getPreferredSize());
		dataNameTextF.setMaximumSize(dataNameTextF.getPreferredSize());
		xPathTextF.setMaximumSize(xPathTextF.getPreferredSize());
		labelCol.setMaximumSize(labelCol.getPreferredSize());

		Component seperator = Box.createRigidArea(new Dimension(20, 10));
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Title: "));
		tempPanel.add(seperator);
		tempPanel.add(titleTextF);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(sumAllCheckBox);
		tempPanel.add(seperator);
		tempPanel.add(groupCheckBox);
		tempPanel.add(seperator);
		tempPanel.add(buildListCheckBox);
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Y-Axis Name: "));
		tempPanel.add(seperator);
		tempPanel.add(a1NameTextF);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(new JLabel("  Y-Axis Node: "));
		tempPanel.add(seperator);
		tempPanel.add(a1TextF);
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("X-Axis Name: "));
		tempPanel.add(seperator);
		tempPanel.add(a2NameTextF);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(new JLabel("  X-Axis Node: "));
		tempPanel.add(seperator);
		tempPanel.add(a2TextF);
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Data Name: "));
		tempPanel.add(seperator);
		tempPanel.add(dataNameTextF);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(new JLabel("Chart Label Column: "));
		tempPanel.add(seperator);
		tempPanel.add(labelCol);
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		JPanel textAreaLabelPanel = new JPanel();
		textAreaLabelPanel.setLayout(new BoxLayout(textAreaLabelPanel, BoxLayout.Y_AXIS));
		textAreaLabelPanel.add(Box.createRigidArea(new Dimension(2, 2)));
		textAreaLabelPanel.add(new JLabel("XPATH: "));
		textAreaLabelPanel.add(Box.createVerticalGlue());
		tempPanel.add(textAreaLabelPanel);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		if(xPathTextF instanceof JTextArea) {
			tempPanel.add(new JScrollPane(xPathTextF));
		} else {
			tempPanel.add(xPathTextF);
		}
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		textAreaLabelPanel = new JPanel();
		textAreaLabelPanel.setLayout(new BoxLayout(textAreaLabelPanel, BoxLayout.Y_AXIS));
		textAreaLabelPanel.add(Box.createRigidArea(new Dimension(2, 2)));
		textAreaLabelPanel.add(new JLabel("Comments: "));
		textAreaLabelPanel.add(Box.createVerticalGlue());
		tempPanel.add(textAreaLabelPanel);
		tempPanel.add(seperator);
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(new JScrollPane(commentsTextA));
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
				nodeLevel = parseAxisLevel(a1TextF.getText());
				axis2Name = a2NameTextF.getText();
				yearLevel = parseAxisLevel(a2TextF.getText());
				var = dataNameTextF.getText();
				labelColumnName = labelCol.getText();
				xPath = xPathTextF.getText();
				sumAll = sumAllCheckBox.isSelected();
				group = groupCheckBox.isSelected();
				setBuildList(buildListCheckBox.isSelected());
				comments = commentsTextA.getText();
				eqEdit.setNewValues(thisGen);
				// only create an undoable edit if the user really made changes 
				// to something rather than just hitting ok
				if(eqEdit.hasRealChanges()) {
					// if the xpath or node level changed we will have
					// to trash the old collapseOnList
					if(eqEdit.didNodeLevelChange() || eqEdit.didXPathChange()) {
						resetCollapseOnList();
					}

					// fake an undo event so that the single query can know if
					// it has to do something about this.
					if(hasSingleQueryExtension()) {
						singleExtension.undoPerformed(new javax.swing.event.UndoableEditEvent(eqEdit, eqEdit));
					}

					// add the edit and refresh the menu
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
	/**
	 * Gets the list types such as sector that this query builder will 
	 * collpase on when creating a table.  If the collapseOnList has not
	 * been initialized it will be then returned.
	 * @return The collpase on list.
	 */
	public List<String> getCollapseOnList() {
		if(collapseOnList == null) {
			// create it
			createCollapseList(qb != null? qb.getDefaultCollpaseList() : new Vector<String>());
		}
		return collapseOnList;
	}
	/**
	 * Creates a list of nodes to collapse on.  This is
	 * based on a list of default values that could be collapsed
	 * on which is pased in.  Then it will look at the types in
	 * the XPath and remove any found from the collapse list. For
	 * example we have default collapse list of: sector, subsector,
	 * technology and an XPath like *[@type = 'sector' and ((@name='electricity') )]
	 * //*[@type = 'technology']/output/node()"
	 *
	 * Only subsector would be left in the list and thus collapsed upon.
	 * @param defaultCollapse Nodes which could be collapsed upon.
	 */
	private void createCollapseList(List<String> defaultCollapse) {
		// copy the list?
		collapseOnList = defaultCollapse;
		Matcher extractType = Pattern.compile("[^\\/]*@type\\s*=\\s*'(\\w+)'\\s*(\\(:\\s*collapse\\s*:\\))?[^\\/]*\\/").matcher(xPath);
		boolean matchesOne = false;
		while(extractType.find()) {
			matchesOne = true;
			if(extractType.group(2) == null) {
				collapseOnList.remove(extractType.group(1));
			} else if(!collapseOnList.contains(extractType.group(1))) {
				// make sure that this is added to the list if not already
				// there
				collapseOnList.add(extractType.group(1));
			}
		}
		if(!matchesOne) {
			// better to not collapse on anything
			// or maybe not 
			//collapseOnList.clear();
		}
	}

	/**
	 * Resets the collapse on this.  This maybe necessary if the xpath
	 * changes.  A subsequent call to getCollapseOnList will have
	 * a newly initiallized collapseOnList
	 */
	public void resetCollapseOnList() {
		collapseOnList = null;
	}

	/**
	 * Parses an axis level to get a nodename and an attribute name.  The
	 * expected format is (node name)[@(attribute name)]. Should there be
	 * no attribute it will be set to null and the usual tries at attribute names
	 * will be tried(usually name or year).
	 * @param axisValue An unparsed axis value with a node name and possible attribute name.
	 * @return A pair with the node name and attribute name for the axis.
	 */
	private DataPair<String, String> parseAxisLevel(String axisValue) {
		int atPos = axisValue.indexOf('@');
		if(atPos == -1) {
			return new DataPair<String, String>(axisValue, null);
		} else {
			return new DataPair<String, String>(axisValue.substring(0, atPos-1),
					axisValue.substring(atPos+1, axisValue.length()-1));
		}
	}

	/**
	 * Takes and axis level and assembles it for display.  It will put it
	 * in the format: (node name)[@(attribute name)].
	 * @param axisLevel A parsed pair of node and attribute names.
	 * @return a String in the correct format.
	 */
	private String displayAxisLevel(DataPair<String, String> axisLevel) {
		if(axisLevel.getValue() == null) {
			return axisLevel.getKey();
		} else {
			return axisLevel.getKey()+"[@"+axisLevel.getValue()+"]";
		}
	}

	public void setGlobal(boolean newGlobal) {
		if(qb != null) {
			qb.isGlobal = newGlobal;
		} else {
			isGlobal = newGlobal;
		}
	}

	/**
	 * Gets a path which will result in a list
	 * of node level values.
	 * @return The path that will give nodeLevel values.
	 */
	public String getNodeLevelPath() {
		if(qb != null) {
			return qb.getNodeLevelPath();
		} else {
			return defaultGetNodeLevelPath();
		}
	}

	/**
	 * Get a path that will only get values when the nodeLevel
	 * value is the value passed in.
	 * @param nodeLevelValue The node level value to filter for.
	 * @return The path that gets the desired result set.
	 */
	public String getForNodeLevelPath(String nodeLevelValue) {
		if(qb != null) {
			return qb.getForNodeLevelPath(nodeLevelValue);
		} else {
			return defaultGetForNodeLevelPath(nodeLevelValue);
		}
	}

	public String defaultGetNodeLevelPath() {
		String nodeLevelAttrName = nodeLevel.getValue();
		if(nodeLevelAttrName == null) {
			nodeLevelAttrName = "name";
		}
		String ret = "/ancestor::*[@type='"+nodeLevel.getKey()+"'";
		if(!shouldUseOnlyType()) {
			ret += " or local-name()='"+nodeLevel.getKey()+"'";
		}
		ret += "]/@"+nodeLevelAttrName;
		return ret;
	}

	public String defaultGetForNodeLevelPath(String nodeLevelValue) {
		String nodeLevelAttrName = nodeLevel.getValue();
		String[] levels = nodeLevelValue.split("/");
		if(nodeLevelAttrName == null) {
			nodeLevelAttrName = "name";
		}
		StringBuilder ret = new StringBuilder("[ancestor::*[(@type='");
		ret.append(nodeLevel.getKey());
		if(shouldUseOnlyType()) {
			ret.append("') and @");
		} else {
			ret.append("' or local-name()='");
			ret.append(nodeLevel.getKey()).append("') and @");
		}
		ret.append(nodeLevelAttrName).append("='").append(levels[levels.length-1]).append("']");
		for(int i = 0; i < levels.length-1; ++i) {
			int colonPos = levels[i].indexOf(':');
			String currType = levels[i].substring(0, colonPos);
			String currVal = levels[i].substring(colonPos+2, levels[i].length());
			ret.append(" and ancestor::*[@type='").append(currType);
			ret.append("' and @name='").append(currVal).append("']");
		}
		ret.append("]");
		return ret.toString();
	}
	private boolean shouldUseOnlyType() {
		// this is a hack to try to speed up the query by omitting the or local-name() = ..
		String nL = nodeLevel.getKey();
		return (nL.equals("sector") || nL.equals("subsector") || nL.equals("technology") || 
				nL.equals("input") || nL.equals("output") || nL.equals("GHG"));
	}

	/**
	 * Return wether or not the single query
	 * extension has been created.
	 * @return True if it has been created, false otherwise.
	 */
	public boolean hasSingleQueryExtension() {
		return singleExtension != null;
	}

	/**
	 * Get the single query extension.  This is only created
	 * if it is requested. If build list is set to false
	 * null will be returned.
	 * @return The single query extension for this query or null if
	 * 	build list is false.
	 */
	public SingleQueryExtension getSingleQueryExtension() {
		if(singleExtension == null && buildSingleQueryList) {
			singleExtension = new SingleQueryExtension(this);
		}
		return singleExtension;
	}

	/**
	 * Reset the QueryBuilder.  The query builder needs to be reset
	 * after a copy/paste mainly becuase it's pointer back to the
	 * QueryGenerator is lost.
	 */
	public void resetQueryBuilder() {
		if(qb == null) {
			return;
		}
		qb.setQueryGenerator(this);
	}
}
