package ModelInterface.ModelGUI2;

import ModelInterface.ConfigurationEditor.guihelpers.XMLFileFilter;
import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.tables.ComboTableModel;
import ModelInterface.ModelGUI2.tables.MultiTableModel;
import ModelInterface.ModelGUI2.tables.CopyPaste;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.MenuAdder;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.io.File;
import java.io.FileWriter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import javax.swing.*;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.geom.Point2D;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.*;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
//import org.w3c.dom.*;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;

import org.jfree.report.JFreeReport;
/*
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.*;
*/
import org.apache.xpath.domapi.XPathEvaluatorImpl;
import org.jfree.chart.JFreeChart;
import org.w3c.dom.xpath.*;

import org.jfree.report.Group;
import org.jfree.report.modules.gui.base.ExportPluginFactory;
import org.jfree.report.JFreeReportBoot;
import org.jfree.report.ElementAlignment;
import org.jfree.report.ReportProcessingException;
import org.jfree.report.modules.gui.base.PreviewDialog;
//import org.jfree.report.elementfactory.TextFieldElementFactory;
import org.jfree.report.elementfactory.DrawableFieldElementFactory;
import org.jfree.ui.FloatDimension;

import org.apache.poi.hssf.usermodel.*;

import com.sleepycat.dbxml.*;

import ModelInterface.InterfaceMain;

public class DbViewer implements ActionListener, MenuAdder {
	private JFrame parentFrame;

	private Document queriesDoc;

	public static XMLDB xmlDB;

	private static String controlStr = "DbViewer";

	private JTable jTable;
		
	private DOMImplementationLS implls;


	protected Vector scns;
	protected JList scnList;
	protected JList regionList;
	protected Vector regions;
	protected BaseTableModel bt;
	protected JScrollPane jsp;
	protected QueryTreeModel queries;

	public DbViewer(JFrame pf) {
		parentFrame = pf;
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr) || evt.getOldValue().equals(controlStr+"Same")) {
						xmlDB.closeDB();
						xmlDB = null;
						try {
							Document tempDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
			.getDOMImplementation().createDocument(null, "queries", null);
							queries.getAsNode(tempDoc);
							//writeDocument(tempDoc, queryFile);
							writeFile(new File(((InterfaceMain)parentFrame).getProperties().getProperty("queryFile"))
								, tempDoc);
						} catch(Exception e) {
							e.printStackTrace();
						}
						parentFrame.getContentPane().removeAll();
					}
					if(evt.getNewValue().equals(controlStr)) {
						readQueries();
						// need to do anything?
					}
				}
			}
		});



		try {
			System.setProperty(DOMImplementationRegistry.PROPERTY,
					"com.sun.org.apache.xerces.internal.dom.DOMImplementationSourceImpl");
					//"org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry reg = DOMImplementationRegistry
					.newInstance();
			implls = (DOMImplementationLS)reg.getDOMImplementation("XML 3.0");
			if (implls == null) {
				System.out.println("Could not find a DOM3 Load-Save compliant parser.");
				JOptionPane.showMessageDialog(parentFrame,
						"Could not find a DOM3 Load-Save compliant parser.",
						"Initialization Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
		} catch (Exception e) {
			System.err.println("Couldn't initialize DOMImplementation: " + e);
			JOptionPane.showMessageDialog(parentFrame,
					"Couldn't initialize DOMImplementation\n" + e,
					"Initialization Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
	}

	private JMenuItem makeMenuItem(String title) {
		JMenuItem ret = new JMenuItem(title);
		ret.addActionListener(this);
		return ret;
	}

	public void addMenuItems(InterfaceMain.MenuManager menuMan) {
		JMenuItem menuItem = new JMenuItem("DB Open");
		menuItem.addActionListener(this);
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).
			getSubMenuManager(InterfaceMain.FILE_OPEN_SUBMENU_POS).addMenuItem(menuItem, 30);

		final JMenuItem menuManage = makeMenuItem("Manage DB");
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addMenuItem(menuManage, 10);
		menuManage.setEnabled(false);
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr)) {
						menuManage.setEnabled(false);
					} 
					if(evt.getNewValue().equals(controlStr)) {
						menuManage.setEnabled(true);
					}
				}
			}
		});
		final JMenuItem menuExpPrn = makeMenuItem("Export / Print");
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addMenuItem(menuExpPrn,  20);
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addSeparator(20);
		menuExpPrn.setEnabled(false);
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr)) {
						menuExpPrn.setEnabled(false);
					}
				} else if(evt.getPropertyName().equals("Query")) {
					menuExpPrn.setEnabled(true);
				}
			}
		});
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("DB Open")) {
			JFileChooser fc = new JFileChooser();
			fc.setDialogTitle("Choose XML Database");

			// Choose only files, not directories
			fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Start in current directory
			fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")));
			//fc.setCurrentDirectory(globalFC.getCurrentDirectory());

			fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
				public boolean accept(File f) {
					return f.getName().toLowerCase().endsWith(".dbxml") || f.isDirectory();
				}
				public String getDescription() {
					return "BDB XML Container (*.dbxml)";
				}
			});

			// Now open chooser
			//int result = fc.showOpenDialog(parentFrame);
			if( fc.showOpenDialog(parentFrame) == JFileChooser.APPROVE_OPTION )  {
				((InterfaceMain)parentFrame).fireControlChange(controlStr);
				doOpenDB(fc);
			}
		} else if(e.getActionCommand().equals("Manage DB")) {
			manageDB();
		} else if(e.getActionCommand().equals("Export / Print")) {
			createReport();
		}
	}

	private void doOpenDB(JFileChooser fc) {
		((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", fc.getCurrentDirectory().toString());
		xmlDB = new XMLDB(fc.getSelectedFile().toString(), parentFrame);
		createTableSelector();
		parentFrame.setTitle("["+fc.getSelectedFile()+"] - ModelInterface");
	}

	private Vector getScenarios() {
		XmlValue temp;
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario", null, null);
			while(res.hasNext()) {
				temp = res.next();
				XmlDocument tempDoc = temp.asDocument();
				ret.add(tempDoc.getName()+" "+XMLDB.getAttr(temp, "name")+ " "+XMLDB.getAttr(temp, "date"));
				tempDoc.delete();
				temp.delete();
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		xmlDB.printLockStats("getScenarios");
		return ret;
	}

	protected Vector getRegions() {
		Vector funcTemp = new Vector<String>(1,0);
		funcTemp.add("distinct-values");
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario/world/region/@name", null, funcTemp);
			while(res.hasNext()) {
				ret.add(res.next().asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.add("Global");
		funcTemp = null;
		xmlDB.printLockStats("getRegions");
		return ret;
	}

	protected QueryTreeModel getQueries() {
		Vector ret = new Vector();
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queriesDoc);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/queries", xpeImpl.createNSResolver(queriesDoc.getDocumentElement())).evaluate(queriesDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		return new QueryTreeModel(res.iterateNext());
	}

	protected String createFilteredQuery(Vector scns, int[] scnSel/*, Vector regions, int[]regionSel*/) {
		StringBuffer ret = new StringBuffer("/");
		boolean added = false;
		for(int i = 0; i < scnSel.length; ++i) {
			String[] attrs = ((String)scns.get(scnSel[i])).split("\\s");
			if(!added) {
				ret.append("scenario[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(attrs[1]).append("' and @date='").append(attrs[2]).append("')");
		}
		ret.append(" ]/world/");
		System.out.println(ret);
		return ret.toString();
		//xmlDB.setQueryFilter(ret.toString());
	}

	protected void createTableSelector() {
		JPanel listPane = new JPanel();
		JLabel listLabel;
		JPanel allLists = new JPanel();
		final JPanel all = new JPanel();
		scns = getScenarios();
		regions = getRegions();
		queries = getQueries();
		scnList = new JList(scns);
		regionList = new JList(regions);
		final JTree queryList = new JTree(queries);
		queryList.setSelectionRow(0);
		for(int i = 0; i < queryList.getRowCount(); ++i) {
			queryList.expandRow(i);
		}
		final JSplitPane sp = new JSplitPane();
		sp.setLeftComponent(null);
		sp.setRightComponent(null);

		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Scenario");
		listPane.add(listLabel);
		JScrollPane listScroll = new JScrollPane(scnList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);

		allLists.setLayout( new BoxLayout(allLists, BoxLayout.X_AXIS));
		allLists.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		allLists.add(listPane);
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Regions");
		listPane.add(listLabel);
		listScroll = new JScrollPane(regionList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		allLists.add(listPane);
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Queries");
		listPane.add(listLabel);
		listScroll = new JScrollPane(queryList);
		listScroll.setPreferredSize(new Dimension(150, 100));
		listPane.add(listScroll);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout( new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.add(Box.createHorizontalGlue());
		JButton createButton = new JButton("Create");
		JButton removeButton = new JButton("Remove");
		final JButton runQueryButton = new JButton("Query");
		final JButton editButton = new JButton("Edit");
		editButton.setEnabled(false);
		runQueryButton.setEnabled(false);
		buttonPanel.add(createButton);
		buttonPanel.add(removeButton);
		buttonPanel.add(editButton);
		buttonPanel.add(runQueryButton);
		listPane.add(buttonPanel);

		allLists.add(listPane);
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		all.add(allLists, BorderLayout.PAGE_START);
		final JPanel tablePanel = new JPanel();
		tablePanel.setLayout( new BoxLayout(tablePanel, BoxLayout.X_AXIS));
		all.add(tablePanel);


		queryList.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				if(queries.isLeaf(e.getPath().getLastPathComponent())) {
					editButton.setEnabled(true);
					runQueryButton.setEnabled(true);
				} else {
					editButton.setEnabled(false);
					runQueryButton.setEnabled(false);
				}
			}
		});

		createButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a Query or Query Group before createing", 
						"Create Query Error", JOptionPane.ERROR_MESSAGE);
					return;
				}

				QueryGenerator qg = new QueryGenerator(parentFrame); 
				if(qg.getXPath().equals("")) {
					return;
				} else if(qg.getXPath().equals("Query Group")) {
					queries.add(queryList.getSelectionPath(), qg.toString());
				} else {
					queries.add(queryList.getSelectionPath(), qg);
				}
				queryList.updateUI();
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a Query or Query Group to Remove", 
						"Query Remove Error", JOptionPane.ERROR_MESSAGE);
				} else {
					queries.remove(queryList.getSelectionPath());
					queryList.updateUI();
				}
			}
		});
		runQueryButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int[] scnSel = scnList.getSelectedIndices();
				int[] regionSel = regionList.getSelectedIndices();
				if(scnSel.length == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select Scenarios to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					//batchQuery(new File("bq.xml"), new File("c:\\test.xls"));
				} else if(regionSel.length == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select Regions to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
				} else if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a query to run", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
				} else {
					String tempFilterQuery = createFilteredQuery(scns, scnSel/*, regions, regionSel*/);
					QueryGenerator qg = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
					parentFrame.getGlassPane().setVisible(true);
					Container ret = null;
					if(qg.isGroup()) {
						ret = createGroupTableContent(qg, tempFilterQuery);
					} else {
						ret = createSingleTableContent(qg, tempFilterQuery);
					}
					if(ret != null) {
						tablePanel.removeAll();
						tablePanel.add(ret);
						((InterfaceMain)parentFrame).fireProperty("Query", null, bt);
					}
					//tablePanel.add(Box.createVerticalGlue());
					parentFrame.getGlassPane().setVisible(false);
					// need old value/new value?
					// fire off property or something we did query
				}
			}
		});

		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a query to edit", 
						"Edit Query Error", JOptionPane.ERROR_MESSAGE);
				} else {
					QueryGenerator tempQG = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
					String oldTitle = tempQG.editDialog();
				}
			}
		});

		Container contentPane = parentFrame.getContentPane();
		contentPane.add(new JScrollPane(all), BorderLayout.PAGE_START);
		parentFrame.setVisible(true);
	}

	private Container createGroupTableContent(QueryGenerator qg, String tempFilterQuery) {
		BaseTableModel btBefore = bt;
		try {
			bt = new MultiTableModel(qg, tempFilterQuery, regionList.getSelectedValues(), parentFrame);
		} catch(NullPointerException e) {
			System.out.println("Warning null pointer while createing MultiTableModel");
			System.out.println("Likely the query didn't get any results");
			bt = btBefore;
			return null;
		}
		btBefore = null;
		jTable = new JTable(bt);
		jTable.setCellSelectionEnabled(true);
		jTable.getColumnModel().getColumn(0).setCellRenderer(((MultiTableModel)bt).getCellRenderer(0,0));
		jTable.getColumnModel().getColumn(0).setCellEditor(((MultiTableModel)bt).getCellEditor(0,0));
		jsp = new JScrollPane(jTable);
		return jsp;
	}

	private Container createSingleTableContent(QueryGenerator qg, String tempFilterQuery) {
		BaseTableModel btBefore = bt;
		try {
			bt = new ComboTableModel(qg, tempFilterQuery, regionList.getSelectedValues(), parentFrame);
		} catch(NullPointerException e) {
			System.out.println("Warning null pointer while createing ComboTableModel");
			System.out.println("Likely the query didn't get any results");
			bt = btBefore;
			return null;
		}
		btBefore = null;
		JFreeChart chart = bt.createChart(0,0);
		//TableSorter sorter = new TableSorter(bt);
		jTable = new JTable(bt);
		new CopyPaste(jTable);
		//sorter.setTableHeader(jTable.getTableHeader());

		jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		jTable.setCellSelectionEnabled(true);

		javax.swing.table.TableColumn col;
		int j = 0;
		while(j < jTable.getColumnCount()) {
			col = jTable.getColumnModel().getColumn(j);
			if(jTable.getColumnName(j).equals("")) {
				col.setPreferredWidth(75);
			} else {
				col.setPreferredWidth(jTable.getColumnName(j).length()*5+30);
			}
			j++;
		}
		BufferedImage chartImage = chart.createBufferedImage(
				350, 350);

		JLabel labelChart = new JLabel();
		labelChart.setIcon(new ImageIcon(chartImage));
		JSplitPane sp = new JSplitPane();
		sp.setLeftComponent(new JScrollPane(jTable));
		sp.setRightComponent(labelChart);
		sp.setDividerLocation(parentFrame.getWidth()-350-15);
		//return sp;
		return jsp = new JScrollPane(sp);
	}
	
	/**
	 * A class which represents a dirty bit.
	 * @author Josh Lurz
	 *
	 */
	private class DirtyBit {
		/**
		 * Whether or not the dirty bit is set.
		 */
		private boolean mIsDirty;
		/**
		 * Constructor which initializes the dirty bit to false.
		 */
		public DirtyBit(){
			mIsDirty = false;
		}
		
		/**
		 * Set the dirty bit.
		 */
		public void setDirty(){
			mIsDirty = true;
		}
		
		/**
		 * Get the value of the dirty bit.
		 * @return Whether the dirty bit is set.
		 */
		public boolean isDirty() {
			return mIsDirty;
		}
	}
	
	private void manageDB() {
		final JDialog filterDialog = new JDialog(parentFrame, "Manage Database", true);
		JPanel listPane = new JPanel();
		JPanel buttonPane = new JPanel();
		JButton addButton = new JButton("Add");
		JButton removeButton = new JButton("Remove");
		JButton exportButton = new JButton("Export");
		JButton doneButton = new JButton("Done");
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		Container contentPane = filterDialog.getContentPane();

		//Vector scns = getScenarios();
		final JList list = new JList(scns);
		
		final DirtyBit dirtyBit = new DirtyBit();
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser fc = new JFileChooser();
				fc.setDialogTitle("Open XML File");

				// Choose only files, not directories
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Start in current directory
				//fc.setCurrentDirectory(globalFC.getCurrentDirectory());
				fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().
						getProperty("lastDirectory", ".")));

				// Set filter for Java source files.
				fc.setFileFilter(new XMLFilter());

				// Now open chooser
				int result = fc.showOpenDialog(parentFrame);

				if (result == JFileChooser.APPROVE_OPTION) {
					dirtyBit.setDirty();
					//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
					((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", 
						 fc.getCurrentDirectory().toString());
					parentFrame.getGlassPane().setVisible(true);
					xmlDB.addFile(fc.getSelectedFile().toString());
					scns = getScenarios();
					list.setListData(scns);
					parentFrame.getGlassPane().setVisible(false);
				}
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Object[] remList = list.getSelectedValues();
				for(int i = 0; i < remList.length; ++i) {
					dirtyBit.setDirty();
					xmlDB.removeDoc(((String)remList[i]).substring(0, 
							((String)remList[i]).indexOf(' ')));
					//System.out.println(((String)remList[i]).substring(0, ((String)remList[i]).indexOf(' ')));
				}
				scns = getScenarios();
				list.setListData(scns);
			}
		});
		exportButton.addActionListener(new ActionListener() {
			/**
			 * Method called when the export button is clicked which allows the
			 * user to select a location to export the scenario to and exports
			 * the scenario.
			 * 
			 * @param aEvent
			 *            The event received.
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent aEvent) {
				Object[] selectedList = list.getSelectedValues();
				for (int i = 0; i < selectedList.length; ++i) {
					File exportLocation = FileUtils.selectFile(parentFrame,
							new XMLFileFilter(), null, true);
					if (exportLocation != null) {
						boolean success = xmlDB.exportDoc(((String) selectedList[i]).substring(0,
								((String) selectedList[i]).indexOf(' ')),
								exportLocation);
						if(success) {
							JOptionPane.showMessageDialog(parentFrame, "Scenario export succeeded.");
						}
						else {
							JOptionPane.showMessageDialog(parentFrame, "Scenario export failed.", null, JOptionPane.ERROR_MESSAGE);
						}
					}
				}
			}

		});
		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(dirtyBit.isDirty()) {
					xmlDB.addVarMetaData(parentFrame);
					scnList.setListData(scns);
					regions = getRegions();
					regionList.setListData(regions);
				}
				filterDialog.setVisible(false);
			}
		});

		buttonPane.setLayout( new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		buttonPane.add(addButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(removeButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(exportButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(doneButton);
		buttonPane.add(Box.createHorizontalGlue());

		JScrollPane sp = new JScrollPane(list);
		sp.setPreferredSize(new Dimension(300, 300));
		listPane.add(new JLabel("Scenarios in Database:"));
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(sp);
		listPane.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(listPane, BorderLayout.PAGE_START);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);
		filterDialog.pack();
		filterDialog.setVisible(true);
	}

	public void createReport() {
		if(jsp == null) {
			// error
			return;
		}
		JFreeReportBoot.getInstance().start();
		JFreeReport report = new JFreeReport();
		java.awt.print.PageFormat pageFormat = new java.awt.print.PageFormat();
		pageFormat.setOrientation(java.awt.print.PageFormat.LANDSCAPE);
		report.setPageDefinition(new org.jfree.report.SimplePageDefinition(pageFormat));
		DrawableFieldElementFactory factory = new DrawableFieldElementFactory();
		Group g = new Group();
		float div = 1;
		int numRows = 0;
		if(jTable.getModel() instanceof MultiTableModel) {
			numRows = (int)jTable.getRowCount()/2;
			div = (float)(jTable.getRowCount()/2);
		} 
		factory.setAbsolutePosition(new Point2D.Float(0, 0));
		factory.setMinimumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
		factory.setMaximumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
		factory.setFieldname("0");
		g.addField("0");
		g.getHeader().addElement(factory.createElement());
		g.getHeader().setPagebreakBeforePrint(true);
		report.addGroup(g);
		final Vector fieldList = new Vector(numRows+1);
		fieldList.add("0");
		for(int i = 1; i < numRows; ++i) {
			g = new Group();
			factory.setFieldname(String.valueOf(i));
			fieldList.add(String.valueOf(i));
			g.setFields(fieldList);
			g.getHeader().addElement(factory.createElement());
			g.getHeader().setPagebreakBeforePrint(true);
			report.addGroup(g);
		}

		report.setData(new javax.swing.table.AbstractTableModel() {
			public int findColumn(String cName) {
				return Integer.parseInt(cName);
			}
			public String getColumnName(int col) {
				return String.valueOf(col);
			}
			public int getColumnCount() {
				return fieldList.size();
			}
			public int getRowCount() {
				return 1;
			}
			public Object getValueAt(int row, int col) {
				final int colf = col;
				return (new org.jfree.ui.Drawable() {
					public void draw(java.awt.Graphics2D graphics, java.awt.geom.Rectangle2D bounds) {
						double scaleFactor = bounds.getWidth() / jsp.getHorizontalScrollBar().getMaximum();
						graphics.scale(scaleFactor, scaleFactor);
						graphics.translate((double)0, 0-bounds.getHeight()*colf);
						if(!(jTable.getModel() instanceof MultiTableModel)) {
							jsp.printAll(graphics);
						} else {
							jTable.printAll(graphics);
						}

						graphics.setColor(Color.WHITE);
						graphics.fillRect(0, (int)bounds.getHeight()*(1+colf), (int)graphics.getClipBounds().getWidth(), (int)bounds.getHeight());
					}
				});
			}
		});

		try {
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.xls.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.plaintext.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.csv.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.html.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.rtf.Enable", "false");
			report.getReportConfiguration().setConfigProperty(MyExcelExportPlugin.enableKey, "true");
			ExportPluginFactory epf = ExportPluginFactory.getInstance();
			MyExcelExportPlugin.bt = bt;
			epf.registerPlugin(MyExcelExportPlugin.class, "20", MyExcelExportPlugin.enableKey);
			PreviewDialog preview = new PreviewDialog(report, parentFrame, true);
			preview.setTitle(parentFrame.getTitle()+" - Export Preview");
			preview.pack();
			preview.setVisible(true);
		} catch(ReportProcessingException e) {
			e.printStackTrace();
		}
	}

	/*
	protected void batchQuery(File queryFile, File excelFile) {
		Document queries = readXMLFile( queryFile );
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queries);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/queries/node()", xpeImpl.createNSResolver(queries.getDocumentElement())).evaluate(queries.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		Node tempNode;
		int[] scnSel;
		HSSFWorkbook wb = null;
		HSSFSheet sheet = null;
		QueryGenerator qgTemp = null;
		Vector tempScns = new Vector();
		Vector tempRegions = new Vector();
		if(excelFile.exists()) {
			try {
				wb = new HSSFWorkbook(new FileInputStream(excelFile));
			} catch (IOException ioe) {
				ioe.printStackTrace();
				return;
			}
		}
		if(wb == null) {
			wb = new HSSFWorkbook();
		}
		while((tempNode = res.iterateNext()) != null) {
			tempScns.removeAllElements();
			tempRegions.removeAllElements();
			NodeList nl = tempNode.getChildNodes();
			for(int i = 0; i < nl.getLength(); ++i) {
				Element currEl = (Element)nl.item(i);
				if(currEl.getNodeName().equals("scenario")) {
					tempScns.add("a "+currEl.getAttribute("name")+' '+currEl.getAttribute("date"));
				} else if(currEl.getNodeName().equals("region")) {
					tempRegions.add(currEl.getAttribute("name"));
				} else {
					qgTemp = new QueryGenerator(currEl);
				}
			}
			scnSel = new int[tempScns.size()];
			for(int i = 0; i < scnSel.length; ++i) {
				scnSel[i] = i;
			}
			createFilteredQuery(tempScns, scnSel);
			sheet = wb.createSheet("Sheet"+String.valueOf(wb.getNumberOfSheets()+1));
			if(qgTemp.isGroup()) {
				(new MultiTableModel(qgTemp, tempRegions.toArray(), parentFrame)).exportToExcel(sheet, wb, sheet.createDrawingPatriarch());
			} else {
				(new ComboTableModel(qgTemp, tempRegions.toArray(), parentFrame)).exportToExcel(sheet, wb, sheet.createDrawingPatriarch());
			}
		}
		try {
			FileOutputStream fos = new FileOutputStream(excelFile);
			wb.write(fos);
			fos.close();
		} catch(IOException ioe) {
			ioe.printStackTrace();
		}
	}
	*/

	public boolean writeFile(File file, Document theDoc) {
		// specify output formating properties
		OutputFormat format = new OutputFormat(theDoc);
		format.setEncoding("UTF-8");
		format.setLineSeparator("\r\n");
		format.setIndenting(true);
		format.setIndent(3);
		format.setLineWidth(0);
		format.setPreserveSpace(false);
		format.setOmitDocumentType(true);

		// create the searlizer and have it print the document

		try {
			FileWriter fw = new FileWriter(file);
			XMLSerializer serializer = new XMLSerializer(fw, format);
			serializer.asDOMSerializer();
			serializer.serialize(theDoc);
			fw.close();
		} catch (java.io.IOException e) {
			System.err.println("Error outputing tree: " + e);
			return false;
		}
		return true;
	}
	public void readQueries() {
		String queryFileName;
		Properties prop = ((InterfaceMain)parentFrame).getProperties();
		// I should probably stop being lazy
		prop.setProperty("queryFile", queryFileName = prop.getProperty("queryFile", "queries.xml"));
		File queryFile = new File(queryFileName);
		if(queryFile.exists()) {
			LSInput lsInput = implls.createLSInput();
			try {
				lsInput.setByteStream(new FileInputStream(queryFile));
			} catch(FileNotFoundException e) {
				// is it even possible to get here
				e.printStackTrace();
			}
			LSParser lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
			queriesDoc = lsParser.parse(lsInput);
		} else {
			//DocumentType DOCTYPE = impl.createDocumentType("recent", "", "");
			queriesDoc = ((DOMImplementation)implls).createDocument("", "queries", null);
		}
	}
}
