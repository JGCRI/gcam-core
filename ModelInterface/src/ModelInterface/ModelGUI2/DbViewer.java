package ModelInterface.ModelGUI2;

import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.tables.ComboTableModel;
import ModelInterface.ModelGUI2.tables.MultiTableModel;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.MenuAdder;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.io.File;
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

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
//import org.w3c.dom.*;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;

import org.jfree.report.JFreeReport;
/*
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
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

	public DbViewer(JFrame pf) {
		parentFrame = pf;
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr)) {
						// clean up
						// remove hook to quit
					}
					if(evt.getNewValue().equals(controlStr)) {
						// hook into quit
					}
				}
			}
		});
		File queryFile = new File("queries.xml");
		try {
			DocumentBuilder parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			if(queryFile.exists()) {
				queriesDoc = parser.parse(new File("queries.xml"));
			} else {
				queriesDoc = parser.getDOMImplementation().createDocument(null, "queries", null);
			}
		} catch(Exception e) {
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
		menuManage.addPropertyChangeListener(new PropertyChangeListener() {
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
		menuExpPrn.addPropertyChangeListener(new PropertyChangeListener() {
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
			System.out.println("HERE");
			((InterfaceMain)parentFrame).fireControlChange(controlStr);
			doOpenDB();
		} else if(e.getActionCommand().equals("Manage DB")) {
			manageDB();
		} else if(e.getActionCommand().equals("Export / Print")) {
			createReport();
		}
	}

	private void doOpenDB() {
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Choose XML Database");

		// Choose only files, not directories
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

		// Start in current directory
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
		int result = fc.showOpenDialog(parentFrame);
		if( result == JFileChooser.APPROVE_OPTION ) {
			//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			/*
			   menuManage.setEnabled(true);
			//menuSave.setEnabled(false);
			//copyMenu.setEnabled(false);
			//pasteMenu.setEnabled(false);
			menuTableFilter.setEnabled(false);
			*/
			xmlDB = new XMLDB(fc.getSelectedFile().toString(), parentFrame);
			createTableSelector();
			//setTitle("["+fc.getSelectedFile()+"] - ModelGUI");
			parentFrame.setTitle("["+fc.getSelectedFile()+"] - ModelInterface");
		}

	}

	private Vector getScenarios() {
		XmlValue temp;
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario");
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
		xmlDB.setQueryFunction("distinct-values(");
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario/world/region/@name");
			while(res.hasNext()) {
				ret.add(res.next().asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.add("Global");
		xmlDB.setQueryFunction("");
		xmlDB.printLockStats("getRegions");
		return ret;
	}

	protected QueryTreeModel getQueries() {
		Vector ret = new Vector();
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queriesDoc);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/queries", xpeImpl.createNSResolver(queriesDoc.getDocumentElement())).evaluate(queriesDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		return new QueryTreeModel(res.iterateNext());
	}

	protected void createFilteredQuery(Vector scns, int[] scnSel/*, Vector regions, int[]regionSel*/) {
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
		xmlDB.setQueryFilter(ret.toString());
	}

	protected Vector scns;
	protected JList scnList;
	protected JList regionList;
	protected Vector regions;
	protected BaseTableModel bt;
	protected JScrollPane jsp;
	protected JSplitPane SP;
	protected QueryTreeModel queries;
	protected void createTableSelector() {
		JPanel listPane = new JPanel();
		JLabel listLabel;
		JPanel allLists = new JPanel();
		final JPanel all = new JPanel();
		scns = getScenarios();
		regions = getRegions();
		queries = getQueries();
		//final JList scnList = new JList(scns);
		scnList = new JList(scns);
		//final JList regionList = new JList(regions);
		regionList = new JList(regions);
		final JTree queryList = new JTree(queries);
		queryList.setSelectionRow(0);
		for(int i = 0; i < queryList.getRowCount(); ++i) {
			queryList.expandRow(i);
		}
		//queryList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		final JSplitPane sp = new JSplitPane();
		sp.setLeftComponent(null);
		sp.setRightComponent(null);

		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Scenario");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
		JScrollPane listScroll = new JScrollPane(scnList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		//listPane.setPreferredSize(new Dimension(150, 250));
		//listPane.add(Box.createVerticalStrut(10));
		//listPane.add(new JSeparator(SwingConstants.HORIZONTAL));

		allLists.setLayout( new BoxLayout(allLists, BoxLayout.X_AXIS));
		allLists.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		allLists.add(listPane);
		//all.add(Box.createHorizontalGlue());
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Regions");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
		listScroll = new JScrollPane(regionList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		allLists.add(listPane);
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Queries");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
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
		//all.setLayout( new BoxLayout(all, BoxLayout.PAGE_AXIS));
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		//all.setOpaque(false);
		//all.setBackground(new Color(200,200,200));
		all.add(allLists, BorderLayout.PAGE_START);
		JPanel ANGRY = new JPanel();
		ANGRY.setLayout( new BoxLayout(ANGRY, BoxLayout.X_AXIS));
		ANGRY.add(sp);
		ANGRY.add(Box.createHorizontalGlue());
		all.add(ANGRY);


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
					// error none selected
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
					// error
					JOptionPane.showMessageDialog(parentFrame, "Please select Scenarios to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					//batchQuery(new File("bq.xml"), new File("c:\\test.xls"));
				} else if(regionSel.length == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select Regions to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a query to run", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else {
					createFilteredQuery(scns, scnSel/*, regions, regionSel*/);
					// table stuff
					//System.out.println(queries.get(queryList.getSelectedIndex()));
					//QueryGenerator qg = (QueryGenerator)queries.get(queryList.getSelectedIndex());
					QueryGenerator qg = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
						parentFrame.getGlassPane().setVisible(true);
					if(qg.isGroup()) {
						bt = new MultiTableModel(qg, regionList.getSelectedValues(), parentFrame);
			jTable = new JTable(bt);
	  		//jTable.getModel().addTableModelListener(thisDemo);

			//jTable.setAutoResizeMode(JTABLE.AUTO_RESIZE_OFF);

			jTable.setCellSelectionEnabled(true);
			jTable.getColumnModel().getColumn(0).setCellRenderer(((MultiTableModel)bt).getCellRenderer(0,0));
			jTable.getColumnModel().getColumn(0).setCellEditor(((MultiTableModel)bt).getCellEditor(0,0));
			int j = 1;
			while( j < jTable.getRowCount()) {
				jTable.setRowHeight(j,200);
				j += 2;
			}
			//jTable.setRowHeight(200);
			//CopyPaste copyPaste = new CopyPaste( jTable );
			jsp = new JScrollPane(jTable);
			sp.setLeftComponent(jsp);
			sp.setDividerLocation(parentFrame.getWidth());
			System.out.println("Should be displaying");
				parentFrame.setVisible(true);
				//menuSave.setEnabled(true);
				//menuExpPrn.setEnabled(true);
						parentFrame.getGlassPane().setVisible(false);
						return;
					}
			//BaseTableModel bt = new ComboTableModel((QueryGenerator)queries.get(queryList.getSelectedIndex()), parentFrame);
			bt = new ComboTableModel(qg, regionList.getSelectedValues(), parentFrame);
			JFreeChart chart = bt.createChart(0,0);
			//TableSorter sorter = new TableSorter(bt);
			jTable = new JTable(bt);
			// Should the listener be set like so..
			//jTable.getModel().addTableModelListener(thisDemo);
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
			//all.add(new JScrollPane(jTable));
			//JSplitPane sp = new JSplitPane();
						/*
						JSplitPane tempSP = new JSplitPane();
			tempSP.setLeftComponent(new JScrollPane(jTable));
			tempSP.setRightComponent(labelChart);
						tempSP.setDividerLocation(((FileChooserDemo)thisFrame).getWidth()-350-15);
						jsp = new JScrollPane(tempSP);
						*/
			sp.setLeftComponent(new JScrollPane(jTable));
			sp.setRightComponent(labelChart);
						sp.setDividerLocation(parentFrame.getWidth()-350-15);
						SP = sp;
						//jsp = new JScrollPane(sp);
						//all.setAlignmentY(Component.LEFT_ALIGNMENT);
						//all.add(Box.createVerticalStrut(10));
			//thisFrame.getContentPane().remove(all);
						//all.add(sp, BorderLayout.CENTER);
						/*
			thisFrame.getContentPane().add(new JScrollPane(sp), BorderLayout.PAGE_START);
						System.out.println(""+thisFrame.getContentPane().getComponentCount());
						System.out.println(thisFrame.getComponent(0));
						*/
				parentFrame.setVisible(true);
				//menuSave.setEnabled(true);
				//menuExpPrn.setEnabled(true);
						parentFrame.getGlassPane().setVisible(false);
				}
			}
		});

		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(parentFrame, "Please select a query to edit", 
						"Edit Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else {
					QueryGenerator tempQG = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
					String oldTitle = tempQG.editDialog();
				}
			}
		});




				Container contentPane = parentFrame.getContentPane();
				/*
				if (splitPane != null) {
					contentPane.remove(splitPane);
				}
				*/
				contentPane.add(new JScrollPane(all), BorderLayout.PAGE_START);
				//contentPane.add(new JScrollPane(all));
				parentFrame.setVisible(true);
	}

	private void manageDB() {
		final JDialog filterDialog = new JDialog(parentFrame, "Manage Database", true);
		JPanel listPane = new JPanel();
		JPanel buttonPane = new JPanel();
		JButton addButton = new JButton("Add");
		JButton removeButton = new JButton("Remove");
		JButton doneButton = new JButton("Done");
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		Container contentPane = filterDialog.getContentPane();

		//Vector scns = getScenarios();
		final JList list = new JList(scns);

		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser fc = new JFileChooser();
				fc.setDialogTitle("Open XML File");

				// Choose only files, not directories
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Start in current directory
				//fc.setCurrentDirectory(globalFC.getCurrentDirectory());

				// Set filter for Java source files.
				fc.setFileFilter(new XMLFilter());

				// Now open chooser
				int result = fc.showOpenDialog(parentFrame);

				if (result == JFileChooser.APPROVE_OPTION) {
					//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
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
					xmlDB.removeDoc(((String)remList[i]).substring(0, 
							((String)remList[i]).indexOf(' ')));
					//System.out.println(((String)remList[i]).substring(0, ((String)remList[i]).indexOf(' ')));
				}
				scns = getScenarios();
				list.setListData(scns);
			}
		});
		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				xmlDB.addVarMetaData(parentFrame);
				scnList.setListData(scns);
				regions = getRegions();
				regionList.setListData(regions);
				filterDialog.setVisible(false);
			}
		});

		buttonPane.setLayout( new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		buttonPane.add(addButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(removeButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(doneButton);
		buttonPane.add(Box.createHorizontalGlue());

		JScrollPane sp = new JScrollPane(list);
		sp.setPreferredSize(new Dimension(300, 300));
		listPane.add(new JLabel("Scenarios is Database:"));
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(sp);
		listPane.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(listPane, BorderLayout.PAGE_START);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);
		filterDialog.pack();
		filterDialog.setVisible(true);
	}

	public void createReport() {
		if(jTable == null || jsp == null || jTable.getRowCount() == 0) {
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
		int numRows = 1;
		if(jTable.getModel() instanceof MultiTableModel) {
			numRows = (int)jTable.getRowCount()/2;
			div = (float)(jTable.getRowCount()/2);
		} else {
			jsp.getVerticalScrollBar().setMaximum((int)SP.getPreferredSize().getHeight());
			jsp.getHorizontalScrollBar().setMaximum((int)SP.getPreferredSize().getWidth());
			jsp.setViewportView(SP);
		}
		factory.setAbsolutePosition(new Point2D.Float(0, 0));
		//factory.setMinimumSize(new FloatDimension((float)800, (float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div))));
		//factory.setMaximumSize(new FloatDimension((float)800, (float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div))));
		//System.out.println("JSP MH: "+jsp.getVerticalScrollBar().getMaximum());
		//System.out.println("SP PF: "+SP.getPreferredSize());
		//System.out.println("H: "+(float)((jsp.getVerticalScrollBar().getMaximum()) /div));
		//System.out.println("Rows: "+jTable.getRowCount());
		/*
		System.out.println("Total Before: "+jTable.getPreferredSize());
		System.out.println("H before: "+(float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div)));
		*/
		factory.setMinimumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
				//		/(jTable.getRowCount()/div))));
		factory.setMaximumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
				//		/(jTable.getRowCount()/div))));
		factory.setFieldname("0");
		g.addField("0");
		g.getHeader().addElement(factory.createElement());
		g.getHeader().setPagebreakBeforePrint(true);
		report.addGroup(g);
		Vector fieldList = new Vector(numRows+1);
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


		/*
		Object[] cNames = {"stuff"};
		Object[][] rowData = new Object[1][1];
		rowData[0][0] = (new org.jfree.ui.Drawable() {
			public void draw(java.awt.Graphics2D graphics, java.awt.geom.Rectangle2D bounds) {
				System.out.println("Got dims: "+bounds);
				graphics.scale(.70,.70);
				System.out.println("Pref size: "+jTable.getPreferredSize());
				jTable.printAll(graphics);
				//graphics.transform(java.awt.geom.AffineTransform.getScaleInstance(.5,1));
				System.out.println("has dims: "+graphics.getClipBounds());
				//graphics.clipRect((int)bounds.getMinX(), (int)bounds.getMinY(), (int)bounds.getWidth(), (int)bounds.getHeight());
			}
		});
		report.setData(new javax.swing.table.DefaultTableModel(rowData, cNames)); 
		*/
		report.setData(new javax.swing.table.AbstractTableModel() {
			public int findColumn(String cName) {
				return Integer.parseInt(cName);
			}
			public String getColumnName(int col) {
				return String.valueOf(col);
			}
			public int getColumnCount() {
				return (int)jTable.getRowCount()/2;
			}
			public int getRowCount() {
				return 1;
			}
			public Object getValueAt(int row, int col) {
				final int colf = col;
				return (new org.jfree.ui.Drawable() {
					public void draw(java.awt.Graphics2D graphics, java.awt.geom.Rectangle2D bounds) {
						double scaleFactor = bounds.getWidth() / jsp.getHorizontalScrollBar().getMaximum();
						//double scaleFactor = bounds.getWidth() / SP.getPreferredSize().getWidth();
						//System.out.println("BNDS: "+bounds);
						//System.out.println("SP PF: "+SP.getPreferredSize());
						//System.out.println("SF: "+scaleFactor);
						graphics.scale(scaleFactor, scaleFactor);
							graphics.translate((double)0, 0-bounds.getHeight()*colf);
						if(!(jTable.getModel() instanceof MultiTableModel)) {
							System.out.println("Printing all");
							//graphics.translate((double)0, 0-20-bounds.getHeight()*colf);
							((JScrollPane)SP.getLeftComponent()).printAll(graphics);
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
			/*
			preview.addWindowListener(new WindowAdapter() {
				public void windowClosing(final WindowEvent e) {
					e.getWindow().setVisible(false);
				}
			});
			*/
			preview.pack();
			preview.setVisible(true);
			//preview.close();
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
}
