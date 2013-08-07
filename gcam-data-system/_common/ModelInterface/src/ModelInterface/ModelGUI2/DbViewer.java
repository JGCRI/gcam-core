package ModelInterface.ModelGUI2;

import ModelInterface.ConfigurationEditor.guihelpers.XMLFileFilter;
import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ModelGUI2.undo.RenameScenarioUndoableEdit;
import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.tables.ComboTableModel;
import ModelInterface.ModelGUI2.tables.MultiTableModel;
import ModelInterface.ModelGUI2.tables.TableSorter;
import ModelInterface.ModelGUI2.tables.CopyPaste;
import ModelInterface.ModelGUI2.tables.TableTransferHandler;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.queries.SingleQueryExtension;
import ModelInterface.ModelGUI2.xmldb.XMLDB;
import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.common.FileChooser;
import ModelInterface.common.FileChooserFactory;
import ModelInterface.common.RecentFilesList.RecentFile;
import ModelInterface.MenuAdder;
import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.QueryResultsPanel;
import ModelInterface.BatchRunner;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.io.File;
import java.io.FileWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import javax.swing.*;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.tree.TreePath;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.filechooser.FileFilter;
import javax.swing.undo.UndoableEdit;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Component;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.util.*;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
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

public class DbViewer implements ActionListener, MenuAdder, BatchRunner {
	private JFrame parentFrame;

	private Document queriesDoc;

	private static String controlStr = "DbViewer";

	private JTable jTable; // does this still need to be a field?

	private DOMImplementationLS implls;


	protected Vector scns;
	protected JList scnList;
	protected JList regionList;
	protected Vector regions;
	protected BaseTableModel bt; // does this still need to be a field?
	protected JScrollPane jsp; // does this still need to be a field?
	protected QueryTreeModel queries;
	private JTabbedPane tablesTabs = new JTabbedPane();
	private JSplitPane scenarioRegionSplit;
	private JSplitPane queriesSplit;
	private JSplitPane tableCreatorSplit;

	public static final String SCENARIO_LIST_NAME = "scenario list";
	public static final String REGION_LIST_NAME = "region list";

	public DbViewer(JFrame pf) {
		parentFrame = pf;
		final DbViewer thisViewer = this;
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr) || evt.getOldValue().equals(controlStr+"Same")) {
						// make sure all queries get killed before we close the database
						for(int tab = 0; tab < tablesTabs.getTabCount(); ++tab) {
							((QueryResultsPanel)tablesTabs.getComponentAt(tab)).killThreadAndWait();
						}

						if(queries.hasChanges() && JOptionPane.showConfirmDialog(
								parentFrame, 
								"The Queries have been modified.  Do you want to save them?",
								"Confirm Save Queries", JOptionPane.YES_NO_OPTION,
								JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
							writeQueries();
						}
						Properties prop = ((InterfaceMain)parentFrame).getProperties();
						prop.setProperty("scenarioRegionSplit", String.valueOf(scenarioRegionSplit.getDividerLocation()));
						prop.setProperty("queriesSplit", String.valueOf(queriesSplit.getDividerLocation()));
						prop.setProperty("tableCreatorSplit", String.valueOf(tableCreatorSplit.getDividerLocation()));
						((InterfaceMain)parentFrame).getUndoManager().discardAllEdits();
						((InterfaceMain)parentFrame).refreshUndoRedo();
						((InterfaceMain)parentFrame).getSaveMenu().removeActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().removeActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(false);
						((InterfaceMain)parentFrame).getSaveMenu().setEnabled(false);
						parentFrame.getContentPane().removeAll();

						// closing the db should be the last thing to do in case
						// other things have pointers to db objects.
						XMLDB.closeDatabase();
					}
					if(evt.getNewValue().equals(controlStr)) {
						String queryFileName;
						Properties prop = ((InterfaceMain)parentFrame).getProperties();
						// I should probably stop being lazy
						prop.setProperty("queryFile", queryFileName = 
							prop.getProperty("queryFile", "standard_queries.xml"));
						// TODO: move to load preferences
						scenarioRegionSplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
						scenarioRegionSplit.setResizeWeight(.5);
						queriesSplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
						//queriesSplit.setLeftComponent(scenarioRegionSplit);
						queriesSplit.setResizeWeight(.5);
						tableCreatorSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, false);
						String tempInt;
						try {
							if((tempInt = prop.getProperty("scenarioRegionSplit")) != null) {
								scenarioRegionSplit.setDividerLocation(Integer.valueOf(tempInt));
							}
							if((tempInt = prop.getProperty("queriesSplit")) != null) {
								queriesSplit.setDividerLocation(Integer.valueOf(tempInt));
							}
							if((tempInt = prop.getProperty("tableCreatorSplit")) != null) {
								tableCreatorSplit.setDividerLocation(Integer.valueOf(tempInt));
							}
						} catch(NumberFormatException nfe) {
							System.out.println("Invalid split location preference: "+nfe);
						}
						((InterfaceMain)parentFrame).getSaveMenu().addActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().addActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(true);
						queriesDoc = readQueries(new File(queryFileName));
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
		/*
		final JMenuItem menuBatch = makeMenuItem("Batch Query");
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addMenuItem(menuBatch, 11);
		menuBatch.setEnabled(false);
		 */
		// TODO: why are there two property change listeners
		final ActionListener thisListener = this;
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr) || 
							evt.getOldValue().equals(controlStr+"Same")) {
						menuManage.setEnabled(false);
						//menuBatch.setEnabled(false);
						// TODO: have the inteface main hanlde all batch files including
						// this ones
						JMenuItem batchMenu = InterfaceMain.getInstance().getBatchMenu();
						batchMenu.removeActionListener(thisListener);
						batchMenu.addActionListener(InterfaceMain.getInstance());
					} 
					if(evt.getNewValue().equals(controlStr)) {
						menuManage.setEnabled(true);
						//menuBatch.setEnabled(true);
						// TODO: have the inteface main hanlde all batch files including
						// this ones
						JMenuItem batchMenu = InterfaceMain.getInstance().getBatchMenu();
						batchMenu.removeActionListener(InterfaceMain.getInstance());
						batchMenu.addActionListener(thisListener);
					}
				}
			}
		});
		final JMenuItem menuExpPrn = makeMenuItem("Export / Print");
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addMenuItem(menuExpPrn,  20);
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).addSeparator(20);
		menuExpPrn.setEnabled(false);
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			private int numQueries = 0;
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr) || 
							evt.getOldValue().equals(controlStr+"Same")) {
						menuExpPrn.setEnabled(false);
					}
				} else if(evt.getPropertyName().equals("Query") && evt.getOldValue() == null) {
					menuExpPrn.setEnabled(true);
					++numQueries;
				} else if(evt.getPropertyName().equals("Query") && evt.getNewValue() == null) {
					if(--numQueries == 0) {
						menuExpPrn.setEnabled(false);
					}
				}
			}
		});
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("DB Open")) {
			File[] dbFiles;
			if(e.getSource() instanceof RecentFile) {
				dbFiles = ((RecentFile)e.getSource()).getFiles();
			} else {
				final FileFilter dbFilter = (new javax.swing.filechooser.FileFilter() {
					public boolean accept(File f) {
						return f.getName().toLowerCase().endsWith(".dbxml") || f.isDirectory();
					}
					public String getDescription() {
						return "BDB XML Container (*.dbxml)";
					}
				});
				FileChooser fc = FileChooserFactory.getFileChooser();
				// Now open chooser
				dbFiles = fc.doFilePrompt(parentFrame, "Choose XML Database", FileChooser.LOAD_DIALOG, 
						new File(((InterfaceMain)parentFrame).getProperties()
								.getProperty("lastDirectory", ".")), dbFilter, this, "DB Open");
			}

			if(dbFiles != null) {
				((InterfaceMain)parentFrame).fireControlChange(controlStr);
				doOpenDB(dbFiles[0]);
			}
		} else if(e.getActionCommand().equals("Manage DB")) {
			manageDB();
		} else if(e.getActionCommand().equals("Batch File")) {
			FileChooser fc = FileChooserFactory.getFileChooser();
			// Now open chooser
			final FileFilter xmlFilter = new XMLFilter();
			File[] batchFiles = fc.doFilePrompt(parentFrame, "Open batch Query File", FileChooser.LOAD_DIALOG, 
					new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")),
					xmlFilter);

			if(batchFiles == null) {
				return;
			} else {
				((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", batchFiles[0].getParent());

				final FileFilter xlsFilter = (new javax.swing.filechooser.FileFilter() {
					public boolean accept(File f) {
						return f.getName().toLowerCase().endsWith(".xls") || f.isDirectory();
					}
					public String getDescription() {
						return "Microsoft Excel File(*.xls)";
					}
				});
				File[] xlsFiles = fc.doFilePrompt(parentFrame, "Select Where to Save Output", FileChooser.SAVE_DIALOG, 
						new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")),
						xlsFilter);
				if(xlsFiles == null) {
					return;
				} else {
					((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", xlsFiles[0].getParent());
					batchQuery(batchFiles[0], xlsFiles[0]);
				}
			}
		} else if(e.getActionCommand().equals("Export / Print")) {
			createReport();
		} else if(e.getActionCommand().equals("Save")) {
			writeQueries();
		} else if(e.getActionCommand().equals("Save As")) {
			final FileFilter xmlFilter = new XMLFilter();
			FileChooser fc = FileChooserFactory.getFileChooser();
			File[] result = fc.doFilePrompt(parentFrame, null, FileChooser.SAVE_DIALOG, 
					new File(((InterfaceMain)parentFrame).getProperties().getProperty("queryFile", ".")),
					xmlFilter);
			if(result != null) {
				File file = result[0];
				if (file.getName().indexOf('.') == -1) {
					if (!(file.getAbsolutePath().endsWith(".xml"))) {
						file = new File(file.getAbsolutePath() + ".xml");
					}
				}
				if (!file.exists() || JOptionPane.showConfirmDialog(null,
						"Overwrite existing file?", "Confirm Overwrite",
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
					((InterfaceMain)parentFrame).getProperties().setProperty("queryFile", 
							file.getAbsolutePath());
					writeQueries();
				}
			}
		}
	}

	private void doOpenDB(File dbFile) {
		((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", dbFile.getParent());
		// put up a wait cursor so that the user knows things are happening while the database loads
		parentFrame.getGlassPane().setVisible(true);
		try {
			XMLDB.openDatabase(dbFile.getAbsolutePath(), parentFrame);
		} catch(Exception e) {
			e.printStackTrace();
			parentFrame.getGlassPane().setVisible(false);
			// tell the user it didn't open.
			JOptionPane.showMessageDialog(parentFrame, "Could not open the xml database.", 
					"DB Open Error", JOptionPane.ERROR_MESSAGE);
			return;
		}

		tablesTabs.setTransferHandler(new TableTransferHandler());
		TabDragListener dragListener = new TabDragListener();
		tablesTabs.addMouseListener(dragListener);
		tablesTabs.addMouseMotionListener(dragListener);

		createTableSelector();
		parentFrame.setTitle("["+dbFile+"] - ModelInterface");
	}

	private Vector getScenarios() {
		XmlValue temp;
		Vector ret = new Vector();
		try {
			XmlResults res = XMLDB.getInstance().createQuery("/scenario", null, null, null);
			while(res.hasNext()) {
				temp = res.next();
				Map<String, String> scnAttrMap = XMLDB.getAttrMap(temp);
				XmlDocument tempDoc = temp.asDocument();
				ret.add(new ScenarioListItem(tempDoc.getName(), scnAttrMap.get("name"), scnAttrMap.get("date")));
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		return ret;
	}

	public void resetScenarioList() {
		scns = getScenarios();
		scnList.setListData(scns);
	}

	protected Vector getRegions() {
		Vector funcTemp = new Vector<String>(1,0);
		funcTemp.add("distinct-values");
		Vector ret = new Vector();
		try {
			XmlResults res = XMLDB.getInstance().createQuery("/scenario/world/"+
					ModelInterface.ModelGUI2.queries.QueryBuilder.regionQueryPortion+"/@name", funcTemp, null, null);
			while(res.hasNext()) {
				ret.add(res.next().asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.add("Global");
		funcTemp = null;
		return ret;
	}

	protected QueryTreeModel getQueries() {
		Vector ret = new Vector();
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queriesDoc);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/queries", xpeImpl.createNSResolver(queriesDoc.getDocumentElement())).evaluate(queriesDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		return new QueryTreeModel(res.iterateNext());
	}

	protected void createTableSelector() {
		JPanel listPane = new JPanel();
		JLabel listLabel;
		JPanel allLists = new JPanel();
		//final JSplitPane all = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		scns = getScenarios();
		regions = getRegions();
		queries = getQueries();
		scnList = new JList(scns);
		scnList.setName(SCENARIO_LIST_NAME);
		regionList = new JList(regions);
		regionList.setName(REGION_LIST_NAME);

		//TODO: get real icons
		final Icon queryIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/group-query.png"));
		final Icon singleQueryIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/single-query.png"));

		// initialize the queries tree
		final JTree queryList = new JTree(queries);
		queryList.setTransferHandler(new QueryTransferHandler(queriesDoc, implls));
		queryList.setDragEnabled(true);
		queryList.getSelectionModel().setSelectionMode(javax.swing.tree.TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
		queryList.setSelectionRow(0);
		for(int i = 0; i < queryList.getRowCount(); ++i) {
			queryList.expandRow(i);
		}
		ToolTipManager.sharedInstance().registerComponent(queryList);
		queryList.setCellRenderer(new DefaultTreeCellRenderer() {
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean sel, boolean expanded, boolean leaf,
					int row, boolean hasFocus) {
				super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
						row, hasFocus);
				if(value instanceof QueryGenerator) {
					setToolTipText(createCommentTooltip(new TreePath(value)));
					setIcon(queryIcon);
				} else if(value instanceof SingleQueryExtension.SingleQueryValue) {
					Object[] tp = new Object[] {
							"root", // will be skipped
							((SingleQueryExtension.SingleQueryValue)value).getParent(),
							value
					};
					setToolTipText(createCommentTooltip(new TreePath(tp)));
					setIcon(singleQueryIcon);
				}
				return this;
			}
		});

		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Scenario");
		listPane.add(listLabel);
		JScrollPane listScroll = new JScrollPane(scnList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);

		allLists.setLayout( new BoxLayout(allLists, BoxLayout.X_AXIS));
		allLists.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		scenarioRegionSplit.setLeftComponent(listPane);
		//allLists.add(listPane);
		//allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Regions");
		listPane.add(listLabel);
		listScroll = new JScrollPane(regionList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		scenarioRegionSplit.setRightComponent(listPane);
		allLists.add(scenarioRegionSplit);
		//allLists.add(listPane);
		//allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listLabel = new JLabel("Queries");
		listPane.add(listLabel);
		listScroll = new JScrollPane(queryList);
		listScroll.setPreferredSize(new Dimension(150, 100));
		listPane.add(listScroll);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout( new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		final JButton createButton = new JButton("Create");
		final JButton removeButton = new JButton("Remove");
		final JButton runQueryButton = new JButton("Run Query");
		final JButton editButton = new JButton("Edit");
		final JButton getSingleQueryButton = new JButton("Update Single Queries"); // TODO: bette name
		editButton.setEnabled(false);
		runQueryButton.setEnabled(false);
		buttonPanel.add(runQueryButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(getSingleQueryButton);
		buttonPanel.add(createButton);
		buttonPanel.add(removeButton);
		buttonPanel.add(editButton);
		listPane.add(buttonPanel);

		queriesSplit.setLeftComponent(scenarioRegionSplit);
		queriesSplit.setRightComponent(listPane);
		allLists.add(queriesSplit);
		//allLists.add(listPane);
		//all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		//all.add(allLists, BorderLayout.PAGE_START);
		tableCreatorSplit.setLeftComponent(allLists);
		/*
		final JPanel tablePanel = new JPanel();
		tablePanel.setLayout( new BoxLayout(tablePanel, BoxLayout.X_AXIS));
		 */
		//final JTabbedPane tablesTabs = new JTabbedPane();
		tableCreatorSplit.setRightComponent(tablesTabs);

		// I have to do this after the lists are in there scrollpanes so I 
		// can ensure they are visible
		if(scns.size() != 0) {
			scnList.setSelectedIndex(scns.size()-1);
			scnList.ensureIndexIsVisible(scns.size()-1);
		}
		if(regions.size() != 0) {
			regionList.setSelectedIndex(0);
		}

		queryList.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				boolean canEdit = true;
				boolean canRun = true;
				boolean canCreate = true;
				boolean canRemove = true;
				if(queryList.getSelectionPaths() != null) {
					for(TreePath path : queryList.getSelectionPaths()) {
						Object selectedObj = path.getLastPathComponent();
						if(selectedObj instanceof QueryGenerator) {
							if(!((QueryGenerator)selectedObj).hasSingleQueryExtension()) {
								// only add the listeners the first time.
								SingleQueryExtension se = ((QueryGenerator)selectedObj)
								.getSingleQueryExtension();
								// could be null if it is not to
								// build list
								if(se != null) {
									queryList.addTreeSelectionListener(se);
									scnList.addListSelectionListener(se);
									regionList.addListSelectionListener(se);

									// make sure it doesn't miss this event
									Object[] selObjScen = scnList.getSelectedValues();
									ScenarioListItem[] selScenarios = 
										new ScenarioListItem[selObjScen.length];
									System.arraycopy(selObjScen, 0, selScenarios, 0, selObjScen.length);

									Object[] selObjRegion = regionList.getSelectedValues();
									String[] selRegions = new String[selObjRegion.length];
									System.arraycopy(selObjRegion, 0, selRegions, 0, selObjRegion.length);
									se.setSelection(selScenarios, selRegions);
									se.valueChanged(e);
								}
							}
						} else if(selectedObj instanceof SingleQueryExtension.SingleQueryValue) {
							canEdit = false;
							if(!((SingleQueryExtension.SingleQueryValue)
									selectedObj).canExecute()) {
								canRun = false;
							}
							canCreate = false;
							canRemove = false;
						} else  {
							canEdit = false;
							canRun = false;
						}
					}
				}
				editButton.setEnabled(canEdit);
				runQueryButton.setEnabled(canRun);
				createButton.setEnabled(canCreate);
				removeButton.setEnabled(canRemove);
			}
		});
		queries.addTreeModelListener(new TreeModelListener() {
			public void treeNodesInserted(TreeModelEvent e) {
				// right now this is the only one I care about
				// so that I can set selection after a node is
				// inserted
				if(!(e.getChildren()[0] instanceof SingleQueryExtension.SingleQueryValue)) {
					TreePath pathWithNewChild = e.getTreePath().pathByAddingChild(e.getChildren()[0]);
					queryList.setSelectionPath(pathWithNewChild);
					queryList.scrollPathToVisible(pathWithNewChild);
				}
			}
			public void treeNodesChanged(TreeModelEvent e) {
				// do nothing..
			}
			public void treeNodesRemoved(TreeModelEvent e) {
				// do nothing..
			}
			public void treeStructureChanged(TreeModelEvent e) {
				// do nothing..
			}
		});

		createButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() != 1) {
					JOptionPane.showMessageDialog(parentFrame, "Please select one Query or Query Group before creating", 
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
					TreePath[] selPaths = queryList.getSelectionPaths();
					for(int i = 0; i < selPaths.length; ++i) {
						queries.remove(selPaths[i]);
					}
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
				} else if(regionSel.length == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select Regions to run the query against", 
							"Run Query Error", JOptionPane.ERROR_MESSAGE);
				} else if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a query to run", 
							"Run Query Error", JOptionPane.ERROR_MESSAGE);
				} else {
					parentFrame.getGlassPane().setVisible(true);
					TreePath[] selPaths = queryList.getSelectionPaths();
					boolean movedTabAlready = false;
					for(int i = 0; i < selPaths.length; ++i) {
						try {
							QueryGenerator qg = null;
							QueryBinding singleBinding = null;
							if(selPaths[i].getLastPathComponent() instanceof QueryGenerator) {
								qg = (QueryGenerator)selPaths[i].getLastPathComponent();
							} else {
								singleBinding = ((SingleQueryExtension.SingleQueryValue)selPaths[i].
										getLastPathComponent()).getAsQueryBinding();
								qg = (QueryGenerator)selPaths[i].getParentPath().getLastPathComponent();
							}
							//add loading icon to QueryResultsPanel
							TabCloseIcon loadingIcon = new TabCloseIcon(tablesTabs);
							JComponent ret = new QueryResultsPanel(qg, singleBinding, parentFrame, scnList.getSelectedValues(), regionList.getSelectedValues(), loadingIcon);

							tablesTabs.addTab(qg.toString(), loadingIcon, ret, createCommentTooltip(selPaths[i])); 
							if(!movedTabAlready) { 
								tablesTabs.setSelectedIndex(tablesTabs.getTabCount()-1);
								movedTabAlready = true; 
							} 


						} catch(ClassCastException cce) {
							System.out.println("Warning: Caught "+cce+" likely a QueryGroup was in the selection");
						}
					}
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
					TreePath[] selPaths = queryList.getSelectionPaths();
					for(int i = 0; i < selPaths.length; ++i) {
						//QueryGenerator tempQG = (QueryGenerator)selPaths[i].getLastPathComponent();
						//tempQG.editDialog();
						queries.doEdit(selPaths[i]);
					}
				}
			}
		});

		getSingleQueryButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				queryList.setSelectionRow(0);
				List<QueryGenerator> qgList = new ArrayList<QueryGenerator>();
				for(int row = 0; row < queryList.getRowCount(); ++row) {
					TreePath currPath = queryList.getPathForRow(row);
					if(currPath.getLastPathComponent() instanceof QueryGenerator) {
						qgList.add((QueryGenerator)currPath.getLastPathComponent());
					}
				}
				createAndShowGetSingleQueries(qgList, scns, regions);
			}
		});

		Container contentPane = parentFrame.getContentPane();
		contentPane.add(tableCreatorSplit/*, BorderLayout.PAGE_START*/);
		//contentPane.add(new JScrollPane(all), BorderLayout.PAGE_START);

		// have to get rid of the wait cursor
		parentFrame.getGlassPane().setVisible(false);

		parentFrame.setVisible(true);
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
		filterDialog.getGlassPane().addMouseListener( new MouseAdapter() {});
		filterDialog.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		final DbViewer thisViewer = this;
		JPanel listPane = new JPanel();
		JPanel buttonPane = new JPanel();
		final JButton addButton = new JButton("Add");
		final JButton removeButton = new JButton("Remove");
		final JButton renameButton = new JButton("Rename");
		final JButton exportButton = new JButton("Export");
		final JButton doneButton = new JButton("Done");
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		Container contentPane = filterDialog.getContentPane();
		removeButton.setEnabled(false);
		renameButton.setEnabled(false);
		exportButton.setEnabled(false);

		//Vector scns = getScenarios();
		final JList list = new JList(scns);

		list.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if(list.getSelectedIndex() == -1) {
					removeButton.setEnabled(false);
					renameButton.setEnabled(false);
					exportButton.setEnabled(false);
				} else {
					removeButton.setEnabled(true);
					renameButton.setEnabled(true);
					exportButton.setEnabled(true);
				}
			}
		});

		final DirtyBit dirtyBit = new DirtyBit();
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				FileChooser fc = FileChooserFactory.getFileChooser();
				final FileFilter xmlFilter = new XMLFilter();
				final File[] xmlFiles = fc.doFilePrompt(parentFrame, "Open XML File", FileChooser.LOAD_DIALOG,
						new File(((InterfaceMain)parentFrame).getProperties().  getProperty("lastDirectory", ".")),
						xmlFilter);

				if(xmlFiles != null) {
					dirtyBit.setDirty();
					((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", 
							xmlFiles[0].getParent());
					final JProgressBar progBar = new JProgressBar(0, xmlFiles.length);
					final JDialog jd = XMLDB.createProgressBarGUI(parentFrame, progBar, "Adding Runs",
					"Importing runs into the database");
					final Runnable incProgress = (new Runnable() {
						public void run() {
							progBar.setValue(progBar.getValue() + 1);
						}
					});
					jd.setVisible(true);
					// run the import off the gui thread which ensures progress updates correctly
					// and keeps the gui responsive
					new Thread(new Runnable() {
						public void run() {
							for(int addFileIndex = 0; addFileIndex < xmlFiles.length; ++addFileIndex) {
								XMLDB.getInstance().addFile(xmlFiles[addFileIndex].getAbsolutePath());
								SwingUtilities.invokeLater(incProgress);
							}
							scns = getScenarios();
							list.setListData(scns);
							jd.setVisible(false);
						}
					}).start();
				}
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Object[] remList = list.getSelectedValues();
				filterDialog.getGlassPane().setVisible(true);
				for(int i = 0; i < remList.length; ++i) {
					dirtyBit.setDirty();
					XMLDB.getInstance().removeDoc(((ScenarioListItem)remList[i]).getDocName());
				}
				scns = getScenarios();
				list.setListData(scns);
				filterDialog.getGlassPane().setVisible(false);
			}
		});
		renameButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				final Object[] renameList = list.getSelectedValues();
				if(renameList.length == 0) {
					return;
				}
				final JDialog renameScenarioDialog = new JDialog(parentFrame, "Rename Scenarios", true);
				renameScenarioDialog.setResizable(false);
				final List<JTextField> renameBoxes = new ArrayList<JTextField>(renameList.length);
				JPanel renameBoxPanel = new JPanel();
				renameBoxPanel.setLayout(new BoxLayout(renameBoxPanel, BoxLayout.Y_AXIS));
				Component verticalSeparator = Box.createVerticalStrut(5);

				for(int i = 0; i < renameList.length; ++i) {
					ScenarioListItem currItem = (ScenarioListItem)renameList[i];
					JPanel currPanel = new JPanel();
					currPanel.setLayout(new BoxLayout(currPanel, BoxLayout.X_AXIS));
					JLabel currLabel = new JLabel("<html>Rename <b>"+currItem.getScnName()+
							"</b> on <b>"+currItem.getScnDate()+"</b> to:</html>");
					JTextField currTextBox = new JTextField(currItem.getScnName(), 20);
					currTextBox.setMaximumSize(currTextBox.getPreferredSize());
					renameBoxes.add(currTextBox);
					currPanel.add(currLabel);
					currPanel.add(Box.createHorizontalGlue());
					renameBoxPanel.add(currPanel);
					currPanel = new JPanel();
					currPanel.setLayout(new BoxLayout(currPanel, BoxLayout.X_AXIS));
					currPanel.add(currTextBox);
					currPanel.add(Box.createHorizontalGlue());
					renameBoxPanel.add(currPanel);
					renameBoxPanel.add(verticalSeparator);
				}

				JPanel renameButtonPanel = new JPanel();
				final JButton renameOK = new JButton("  OK  ");
				final JButton renameCancel = new JButton("Cancel");
				renameButtonPanel.setLayout(new BoxLayout(renameButtonPanel, BoxLayout.X_AXIS));
				renameButtonPanel.add(Box.createHorizontalGlue());
				renameButtonPanel.add(renameOK);
				renameButtonPanel.add(renameCancel);
				ActionListener renameButtonListener = new ActionListener() {
					public void actionPerformed(ActionEvent renameEvt) {
						if(renameEvt.getSource() == renameOK) {
							for(int i = 0; i < renameList.length; ++i) {
								ScenarioListItem currItem = (ScenarioListItem)renameList[i];
								String currText = renameBoxes.get(i).getText(); 
								// only do it if the name really is different
								if(!currItem.getScnName().equals(currText)) {
									// the undoable edit will take care of doing
									// the rename
									UndoableEdit renameEdit = new RenameScenarioUndoableEdit(thisViewer, 
											currItem, currText);
									InterfaceMain.getInstance().getUndoManager().addEdit(renameEdit);
									InterfaceMain.getInstance().refreshUndoRedo();
								}
							}
						}
						//scns = getScenarios();
						list.setListData(scns);
						renameScenarioDialog.dispose();
					}
				};
				renameOK.addActionListener(renameButtonListener);
				renameCancel.addActionListener(renameButtonListener);

				renameBoxPanel.add(renameButtonPanel);
				renameBoxPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
				renameScenarioDialog.getContentPane().add(renameBoxPanel);
				renameScenarioDialog.pack();
				renameScenarioDialog.setVisible(true);
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
				final Object[] selectedList = list.getSelectedValues();
				final boolean isSingleSelection = selectedList.length == 1;
				FileFilter fileFilter;
				String saveDialogTitle;
				if(isSingleSelection) {
					fileFilter = new XMLFileFilter();
					saveDialogTitle = "Save As XML";
				} else {
					fileFilter = new FileFilter() {
						public boolean accept(File f) {
							return f.isDirectory();
						}
						public String getDescription() {
							return "Directory to export into";
						}
					};
					saveDialogTitle = "Select Export Directory";
				}
				FileChooser fc = FileChooserFactory.getFileChooser();
				final File[] exportLocation = fc.doFilePrompt(parentFrame, saveDialogTitle, FileChooser.SAVE_DIALOG,
						new File(((InterfaceMain)parentFrame).getProperties().  getProperty("lastDirectory", ".")),
						fileFilter);
				if(isSingleSelection && !exportLocation[0].getName().endsWith(".xml")) {
					exportLocation[0] = new File(exportLocation[0].getParentFile(), 
							exportLocation[0].getName()+".xml");
				}
				if (exportLocation == null) {
					// user canceled, nothing to do
					return;
				}
				final JProgressBar progBar = new JProgressBar(0, selectedList.length);
				final JDialog jd = XMLDB.createProgressBarGUI(parentFrame, progBar, "Exporting Runs",
				"Exporting runs from the database");
				final Runnable incProgress = (new Runnable() {
					public void run() {
						progBar.setValue(progBar.getValue() + 1);
					}
				});
				jd.setVisible(true);
				// run the export off the gui thread which ensures progress updates correctly
				// and keeps the gui responsive
				new Thread(new Runnable() {
					public void run() {
						boolean success = true;
						for (int i = 0; i < selectedList.length; ++i) {
							ScenarioListItem currItem = (ScenarioListItem)selectedList[i];
							File exportFile;
							if(isSingleSelection) {
								exportFile = exportLocation[0];
							} else {
								String exportFileName = currItem.getScnName()+"_"+
								currItem.getScnDate().replaceAll(":", "_")+".xml";
								exportFile = new File(exportLocation[0], exportFileName);
							}
							success = success && XMLDB.getInstance()
							.exportDoc(currItem.getDocName(), 
									exportFile);
							SwingUtilities.invokeLater(incProgress);
						}
						jd.setVisible(false);
						if(success) {
							JOptionPane.showMessageDialog(parentFrame, "Scenario export succeeded.");
						}
						else {
							JOptionPane.showMessageDialog(parentFrame, "Scenario export failed.", null, JOptionPane.ERROR_MESSAGE);
						}
					}
				}).start();
			}

		});
		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(dirtyBit.isDirty()) {
					// meta data now set on demand
					//xmlDB.addVarMetaData(parentFrame);
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
		buttonPane.add(renameButton);
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
		if(tablesTabs.getTabCount() == 0) {
			// error?
			return;
		}
		jsp = (JScrollPane)tablesTabs.getSelectedComponent();
		jTable = getJTableFromComponent(jsp);
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
			//MyExcelExportPlugin.bt = bt;
			MyExcelExportPlugin.bt = (BaseTableModel)jTable.getModel();
			epf.registerPlugin(MyExcelExportPlugin.class, "20", MyExcelExportPlugin.enableKey);
			PreviewDialog preview = new PreviewDialog(report, parentFrame, true);
			preview.setTitle(parentFrame.getTitle()+" - Export Preview");
			preview.pack();
			preview.setVisible(true);
		} catch(ReportProcessingException e) {
			e.printStackTrace();
		}
	}

	protected BatchWindow batchQuery(File queryFile, final File excelFile) {
		final Vector tempScns = getScenarios();
		final String singleSheetCheckBoxPropName = "batchQueryResultsInDifferentSheets";
		final String includeChartsPropName ="batchQueryIncludeCharts";
		final String splitRunsPropName = "batchQuerySplitRunsInDifferentSheets";
		final String replaceResultsPropName = "batchQueryReplaceResults";
		Properties prop = InterfaceMain.getInstance().getProperties();

		// Create a Select Scenarios dialog to get which scenarios to run
		final JList scenarioList = new JList(tempScns); 
		final JDialog scenarioDialog = new JDialog(parentFrame, "Select Scenarios to Run", true);
		JPanel listPane = new JPanel();
		JPanel buttonPane = new JPanel();
		final JCheckBox singleSheetCheckBox = new JCheckBox("Place all results in different sheets",
				Boolean.parseBoolean(prop.getProperty(singleSheetCheckBoxPropName, "false")));
		final JCheckBox drawPicsCheckBox = new JCheckBox("Include charts with results",
				Boolean.parseBoolean(prop.getProperty(includeChartsPropName, "true")));
		final JCheckBox seperateRunsCheckBox = new JCheckBox("Split runs into different sheets",
				Boolean.parseBoolean(prop.getProperty(splitRunsPropName, "false")));
		final JButton okButton = new JButton("Ok");
		okButton.setEnabled(false);
		JButton cancelButton = new JButton("Cancel");
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		Container contentPane = scenarioDialog.getContentPane();
		final JCheckBox overwriteCheckBox = new JCheckBox("Overwrite selected file if it exists",
				Boolean.parseBoolean(prop.getProperty(replaceResultsPropName, "false")));

		scenarioList.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if(scenarioList.isSelectionEmpty()) {
					okButton.setEnabled(false);
				} else {
					okButton.setEnabled(true);
				}
			}
		});

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				scenarioDialog.dispose();
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				scenarioList.clearSelection();
				scenarioDialog.dispose();
			}
		});

		buttonPane.setLayout( new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(okButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(cancelButton);

		JScrollPane sp = new JScrollPane(scenarioList);
		sp.setPreferredSize(new Dimension(300, 300));
		listPane.add(new JLabel("Select Scenarios:"));
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(sp);
		listPane.add(singleSheetCheckBox);
		listPane.add(overwriteCheckBox);
		listPane.add(drawPicsCheckBox);
		listPane.add(seperateRunsCheckBox);
		listPane.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(listPane, BorderLayout.PAGE_START);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);
		scenarioDialog.pack();
		scenarioDialog.setVisible(true);

		if(scenarioList.isSelectionEmpty()) {
			return null;
		}
		// save the check box options back into the properties
		prop.setProperty(singleSheetCheckBoxPropName, Boolean.toString(singleSheetCheckBox.isSelected()));
		prop.setProperty(includeChartsPropName, Boolean.toString(drawPicsCheckBox.isSelected()));
		prop.setProperty(splitRunsPropName, Boolean.toString(seperateRunsCheckBox.isSelected()));
		prop.setProperty(replaceResultsPropName, Boolean.toString(overwriteCheckBox.isSelected()));

		// read the batch query file
		Document queries = readQueries( queryFile );
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queries);
		final XPathResult res = (XPathResult)xpeImpl.createExpression("/queries/node()", xpeImpl.createNSResolver(queries.getDocumentElement())).evaluate(queries.getDocumentElement(), XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);

		final int numQueries = res.getSnapshotLength();
		if(numQueries == 0) {
			JOptionPane.showMessageDialog(parentFrame, "Could not find queries to run in batch file:\n"+queryFile,
					"Batch Query Error", JOptionPane.ERROR_MESSAGE);
			return null;
		}
		final Vector<Object[]> toRunScns = new Vector<Object[]>();
		if(!seperateRunsCheckBox.isSelected()) {
			toRunScns.add(scenarioList.getSelectedValues());
		} else {
			for(Object currScn : scenarioList.getSelectedValues()) {
				Object[] temp = new Object[1];
				temp[0] = currScn;
				toRunScns.add(temp);
			}
		}
		//create window

		final BatchWindow bWindow = new BatchWindow(excelFile, toRunScns, singleSheetCheckBox, drawPicsCheckBox, 
				numQueries,res, parentFrame, overwriteCheckBox);
		//create listener for window


        return bWindow;
	}

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
	public Document readQueries(File queryFile) {
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
			return lsParser.parse(lsInput);
		} else {
			//DocumentType DOCTYPE = impl.createDocumentType("recent", "", "");
			return ((DOMImplementation)implls).createDocument("", "queries", null);
		}
	}
	private void writeQueries() {
		try {
			Document tempDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
			.getDOMImplementation().createDocument(null, "queries", null);
			queries.getAsNode(tempDoc);
			//writeDocument(tempDoc, queryFile);
			writeFile(new File(((InterfaceMain)parentFrame).getProperties().getProperty("queryFile"))
			, tempDoc);
			queries.resetChanges();
		} catch(ParserConfigurationException pce) {
			// TODO: error to the sceen that it could no save..
			pce.printStackTrace();
		}
	}
	public static BaseTableModel getTableModelFromComponent(java.awt.Component comp) {
		Object c;
		try {
			c = ((QueryResultsPanel)comp).getComponent(0);
			//If a JPanel is returned, QueryResultsPanel returned a Panel with text,
			//so no table can be extracted
			if(c instanceof JPanel){
				return null;
			}
			if(c instanceof JSplitPane) {
				return (BaseTableModel)((TableSorter)((JTable)((JScrollPane)((JSplitPane)c).getLeftComponent()).getViewport().getView()).getModel()).getTableModel();
			} else {
				return (BaseTableModel)((JTable)((JScrollPane)c).getViewport().getView()).getModel();
			}
		} catch (ClassCastException e) {
			e.printStackTrace();
			return null;
		}
	}
	public static JTable getJTableFromComponent(java.awt.Component comp) {
		Object c;
		try {
			QueryResultsPanel qPanel = (QueryResultsPanel)comp;
			c = ((JScrollPane)qPanel.getComponent(0)).getViewport().getView();
			if(c instanceof JSplitPane) {
				return (JTable)((JScrollPane)((JSplitPane)c).getLeftComponent()).getViewport().getView();
			} else {
				return (JTable)c;
			}
		} catch (ClassCastException e) {
			e.printStackTrace();
			return null;
		}
	}
	private String createCommentTooltip(TreePath path) {
		QueryGenerator qg;
		if(path.getLastPathComponent() instanceof QueryGenerator) {
			qg = (QueryGenerator)path.getLastPathComponent();
		} else {
			// SingleQueryValue..
			qg = (QueryGenerator)path.getParentPath().getLastPathComponent();
		}
		StringBuilder ret = new StringBuilder("<html><table cellpadding=\"2\"><tr><td>");
		for(int i = 1; i < path.getPathCount() -1; ++i) {
			ret.append(path.getPathComponent(i)).append(":<br>");
		}
		ret.append(path.getLastPathComponent()).append("<br><br>Comments:<br>")
		.append(qg.getComments()).append("</td></tr></table></html>");
		return ret.toString();
	}
	private class TabDragListener implements MouseListener, MouseMotionListener {
		MouseEvent firstMouseEvent = null;
		public void mousePressed(MouseEvent e) {
			if(tablesTabs.getTabCount() > 0 && 
					tablesTabs.getBoundsAt(tablesTabs.getSelectedIndex()).contains(e.getPoint())) {
				firstMouseEvent = e;
				e.consume();
			}
		}
		public void mouseDragged(MouseEvent e) {
			// make sure that there was a press first and that that tab has not
			// since been closed
			if(firstMouseEvent != null && tablesTabs.getTabCount() > 0 &&
					tablesTabs.getBoundsAt(tablesTabs.getSelectedIndex()).contains(e.getPoint())) {
				e.consume();

				//TODO: maybe cut would be possible, for now just copy
				// if we do cut we would probably want to do ctrl mask
				// for cut not paste.
				int action = TransferHandler.COPY;

				int dx = Math.abs(e.getX() - firstMouseEvent.getX());
				int dy = Math.abs(e.getY() - firstMouseEvent.getY());
				// Arbitrarily define a 5-pixel shift as the
				// official beginning of a drag.
				if (dx > 5 || dy > 5) {
					JComponent c = (JComponent)e.getSource();
					//Tell the transfer handler to initiate the drag.
					tablesTabs.getTransferHandler().exportAsDrag(tablesTabs, 
							firstMouseEvent, action);
					firstMouseEvent = null;
				}

			}
		}
		// all the events we don't care about..
		public void mouseMoved(MouseEvent e) {}
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mouseClicked(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) {}
	}

	/**
	 * Creates a dialog which will ask for scenarios, regions, and
	 * queries to scan for SingleQueryValues.  When finished selecting these
	 * values it will do the scan.  This could take some time and the rest of the
	 * GUI should be inoperable while the scan is occuring.  A progress bar will
	 * be displayed.
	 * @param queries All of the queries in the tree
	 * @param scenarios All of the scenarios.
	 * @param regions All of the regions.
	 */
	private void createAndShowGetSingleQueries(final List<QueryGenerator> queries, final List<ScenarioListItem> scenarios,
			final List<String> regions) {
		// create the dialog which will block the rest of the gui until it is done
		final JDialog scanDialog = new JDialog(parentFrame, "Update Single Query Cache", true);
		final JTabbedPane selectionTabs = new JTabbedPane();

		// JLists expects these as arrays so create them now
		final ScenarioListItem[] scenariosArr = new ScenarioListItem[scenarios.size()];
		final String[] regionsArr = new String[regions.size()];
		final QueryGenerator[] queriesArr = new QueryGenerator[queries.size()];
		scenarios.toArray(scenariosArr);
		regions.toArray(regionsArr);
		queries.toArray(queriesArr);

		// create the display components
		final JList selectScenarios = new JList(scenariosArr);
		final JList selectRegions = new JList(regionsArr);
		final JList selectQueries = new JList(queriesArr);
		final JButton scanButton = new JButton("Scan");
		final JButton cancelButton = new JButton("Cancel");
		final JPanel all = new JPanel();
		final Component seperator = Box.createRigidArea(new Dimension(20, 10));

		// create the progress bar
		final JProgressBar scanProgress = new JProgressBar(0, queries.size());
		final JLabel progLabel = new JLabel("Label");
		// processing should be done off of the gui thread to ensure responsiveness
		final Thread scanThread = new Thread(new Runnable() {
			public void run() {
				// increasing progress should be run on the gui thread so I will create
				// this runnable and use the SwingUtilities.invokeLater to run it on there
				final Runnable incProgress = new Runnable() {
					public void run() {
						scanProgress.setValue(scanProgress.getValue()+1);
					}
				};

				// make lists of the selected values only
				int[] selIndexes = selectScenarios.getSelectedIndices();
				final ScenarioListItem[] selScenarios = new ScenarioListItem[selIndexes.length];
				int pos = 0;
				for(int selIndex : selIndexes) {
					selScenarios[pos++] = scenariosArr[selIndex];
				}

				selIndexes = selectRegions.getSelectedIndices();
				final String[] selRegions = new String[selIndexes.length];
				pos = 0;
				for(int selIndex: selIndexes) {
					selRegions[pos++] = regionsArr[selIndex];
				}

				selIndexes = selectQueries.getSelectedIndices();
				final List<QueryGenerator> selQueries = new ArrayList<QueryGenerator>(selIndexes.length);
				for(int selIndex : selIndexes) {
					selQueries.add(queriesArr[selIndex]);
				}
				scanProgress.setMaximum(selIndexes.length);

				// get the cache document, if there is an exception getting it then it 
				// may not exsist so we can try to create it
				XmlDocument doc = null;
				XMLDB xmldbInstance = XMLDB.getInstance();
				try {
					doc = xmldbInstance.getDocument("cache");
				} catch(XmlException e) {
					// might not exsist yet so create it.
					doc = xmldbInstance.createDocument("cache", "<singleQueryListCache />");
				}
				// if it is still null there is a real problem so notify the user that it
				// is not going to work and return
				if(doc == null) {
					scanDialog.setVisible(false);
					JOptionPane.showMessageDialog(parentFrame,
							"Could not get cache from the database.",
							"Cache Error", JOptionPane.ERROR_MESSAGE);
					return;
				}

				boolean wasInterrupted = false;

				// for each query that is enabled have the extension create and cache it's 
				// single query list.  The cache will be set as metadata on the cache doc
				// if we got interrupted we must stop now
				for(Iterator<QueryGenerator> it = selQueries.iterator(); it.hasNext() && !wasInterrupted; ) {
					QueryGenerator currQG = it.next();
					progLabel.setText("Scanning "+currQG.toString());
					SingleQueryExtension se = currQG.getSingleQueryExtension();
					// could be null if the extension is not enabled
					if(se != null) {
						se.createSingleQueryListCache(doc, selScenarios, selRegions);
					}
					SwingUtilities.invokeLater(incProgress);
					wasInterrupted = Thread.interrupted();
				}

				// don't forget to write the changes to the cache back to the db
				// but only if we were not canceled
				if(!wasInterrupted) {
					xmldbInstance.updateDocument(doc);
				}

				// clean up and take down the progress bar
				scanDialog.setVisible(false);
			}
		});

		// default is to select all
		selectScenarios.setSelectionInterval(0, scenariosArr.length-1);
		selectRegions.setSelectionInterval(0, regionsArr.length-1);
		selectQueries.setSelectionInterval(0, queriesArr.length-1);

		// create the tabs for the selections
		selectionTabs.addTab("Scenarios", new JScrollPane(selectScenarios));
		selectionTabs.addTab("Regions", new JScrollPane(selectRegions));
		selectionTabs.addTab("Queries", new JScrollPane(selectQueries));
		// have it take as much room as possible
		selectionTabs.setPreferredSize(new Dimension(400, 400));

		// need to make sure the label will align to the left
		final JPanel labelPanel = new JPanel();
		labelPanel.setLayout(new BoxLayout(labelPanel, BoxLayout.X_AXIS));
		labelPanel.add(progLabel);
		labelPanel.add(Box.createHorizontalGlue());

		// buttons need to be layouted out horizontally
		final JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(scanButton);
		buttonPanel.add(seperator);
		buttonPanel.add(cancelButton);

		// the cancel button will interrupt the can if it is running
		// or just close the dialog if it is not.  note that if the 
		// users cancels NONE of the scan will be written back to the
		// database
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(scanThread.isAlive()) {
					progLabel.setText("Canceling Scan");
					scanThread.interrupt();
					// this is in effect interrupting all single queries create list
					// queries
					int[] selIndexes = selectQueries.getSelectedIndices();
					for(int selIndex : selIndexes) {
						queriesArr[selIndex].getSingleQueryExtension().interruptGatherThread();
					}
					// will let the scan thread hide the dialog
				} else {
					// has not started yet so just hide it
					scanDialog.setVisible(false);
				}
			}
		});

		// when the scan button is hit we will switch the content of the dialog
		// from the selection lists to a progress bar to let the user know how
		// things are going.  The user will still be able to cancel once the scan
		// starts
		scanButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				scanButton.setEnabled(false);

				// set up the new content pane
				final JPanel progPanel = new JPanel();
				progPanel.setLayout(new BoxLayout(progPanel, BoxLayout.Y_AXIS));
				progPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
				progPanel.add(scanProgress);
				progPanel.add(labelPanel);
				// make sure it is atleast 200 accross
				progPanel.add(Box.createHorizontalStrut(300));
				progPanel.add(Box.createVerticalGlue());
				progPanel.add(new JSeparator(SwingConstants.HORIZONTAL));
				progPanel.add(seperator);
				progPanel.add(buttonPanel);

				// start scanning to cache queries
				scanThread.start();

				// display the new pane and shrink down any unnessary space
				scanDialog.setContentPane(progPanel);
				scanDialog.pack();
			}
		});

		// create the layout which will be tabbed pane on top and buttons on the
		// bottom
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		all.add(selectionTabs);
		all.add(seperator);
		all.add(new JSeparator(SwingConstants.HORIZONTAL));
		all.add(seperator);
		all.add(buttonPanel);

		// set the content pane for the dialog and show it
		scanDialog.setSize(400, 400);
		scanDialog.setResizable(false);
		scanDialog.setContentPane(all);
		scanDialog.setVisible(true);
	}
	public void runBatch(Node command) {
		NodeList children = command.getChildNodes();
		for(int i = 0; i < children.getLength(); ++i ) {
			Node child = children.item(i);
			// TODO: put in a parse filter for this
			if(child.getNodeType() != Node.ELEMENT_NODE) {
				continue;
			}
			String actionCommand = ((Element)child).getAttribute("name");
			if(actionCommand == null) {
				continue;
			}
			if(actionCommand.equals("XMLDB Batch File")) {
				File queryFile = null;
				File outFile = null;
				String dbFile = null;
				//ArrayList<ScenarioListItem> scenarios = new ArrayList<ScenarioListItem>();
				// read file names for header file, csv files, and the output file
				NodeList fileNameChildren = child.getChildNodes();
				for(int j = 0; j < fileNameChildren.getLength(); ++j) {
					Node fileNode = fileNameChildren.item(j);
					if(fileNode.getNodeType() != Node.ELEMENT_NODE) {
						continue;
					}
					if(fileNode.getNodeName().equals("queryFile")) {
						queryFile = new File(fileNode.getTextContent());
					} else if(fileNode.getNodeName().equals("outFile")) {
						outFile = new File(fileNode.getTextContent());
					} else if(fileNode.getNodeName().equals("xmldbLocation")) {
						//dbFile = new File(fileNode.getTextContent());
                        dbFile = fileNode.getTextContent();
                        /*
					} else if(fileNode.getNodeName().equals("scenario")) {
                        scenarios.add(new ScenarioListItem("", ((Element)fileNode).getAttribute("name"),
                                    ((Element)fileNode).getAttribute("date")));
                                    */
					} else {
						System.out.println("Unknown tag: "+fileNode.getNodeName());
						// should I print this error to the screen?
					}
				}
				// make sure we have enough to run the batch query 
				// which means we have a query file, output file, and
				// at database location
				if(queryFile != null && outFile != null && dbFile != null /*&& !scenarios.isEmpty()*/) {
                    try {
                        XMLDB.openDatabase(dbFile, parentFrame);

                        // run the queries and wait for them to finish so that we
                        // can close the database
                        BatchWindow runner = batchQuery(queryFile, outFile);
                        if(runner != null) {
                            runner.waitForFinish();
                        }
                    } catch(Exception e) {
                        e.printStackTrace();
                        JOptionPane.showMessageDialog(parentFrame,
                                "Recieved error while running: "+e.getMessage(),
                                "Batch File Error", JOptionPane.ERROR_MESSAGE);
                    } finally {
                        XMLDB.closeDatabase();
                    }
				} else {
					JOptionPane.showMessageDialog(parentFrame,
							"Not enough info to run batch query.",
							"Batch File Error", JOptionPane.ERROR_MESSAGE);
				}
			} else {
				System.out.println("Unknown command: "+actionCommand);
				JOptionPane.showMessageDialog(parentFrame,
						"Unknown command: "+actionCommand,
						"Batch File Error", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
