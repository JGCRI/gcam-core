package ModelInterface.ModelGUI2.tables;

import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.undo.FlipUndoableEdit;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;
import java.util.Iterator;

import javax.swing.JTable;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.tree.TreePath;
import javax.swing.table.TableCellRenderer;
import javax.swing.undo.UndoManager;

import org.apache.poi.hssf.usermodel.*;

import java.awt.image.BufferedImage;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.xpath.XPathExpression;
import org.w3c.dom.xpath.XPathResult;

import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class NewDataTableModel extends BaseTableModel{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Vector indCol;
	Vector indRow;
	String ind1Name;
	String ind2Name;
	Map data;
	boolean flipped;
	String w3;
	JFreeChart chart;
	TableCellRenderer documentationRenderer;
	int chartLabelCol = -1;

	/**
	 * Constructor initializes data members, and calls buildTable to initialize data, and filterMaps
	 * @param tp the Tree Path which was selected from the tree, needed to build table
	 *        doc needed to run the XPath query against
	 *        parentFrame needed to create dialogs
	 *        tableTypeString to be able to display the type of table this is
	 */
	public NewDataTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString, Documentation documentationIn) {
		super(tp, doc, parentFrame, tableTypeString, documentationIn);
		indCol = new Vector();
		indRow = new Vector();
		data = new TreeMap();
		w3 = "";
		wild = chooseTableHeaders(tp/*, parentFrame*/);
	        wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
	        wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 1));
		indCol.add(0,w3 /*set2Name*/);
		ind1Name = (String)wild.get(0);
		ind2Name = (String)wild.get(1);
		flipped = false;
		documentationRenderer = getDocumentationRenderer();
	}

	public TableCellRenderer getCellRenderer(int row, int col) {
		if(col == 0) {
			return null;
		} else {
			return documentationRenderer;
		}
	}

	/**
	 * Uses an XPath expression to get a set of data nodes, then figures out which attrubute values of the 
	 * axes define where to find the node, and stores it in the data map
	 * @param xpe an XPath expression which will be evaluated to get a set of nodes
	 */
	protected void buildTable(XPathExpression xpe) {
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  // TreeSets don't allow duplicates
	  TreeSet col = new TreeSet();
	  TreeSet row = new TreeSet();
	  Node tempNode;
	  // region and year isn't a good name anymore
	  // more like axis keys
	  Object[] regionAndYear;
	  while ((tempNode = res.iterateNext()) != null) {
		regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode());
		col.add(regionAndYear[0]);
		row.add(regionAndYear[1]);
		// colKey;rowKey maps to the data that should go in that cell
		data.put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
		if(units == null) {
			units = ((Element)tempNode.getParentNode()).getAttribute("unit");
		}
	  }
	  indCol = new Vector(col);
	  indRow = new Vector(row);
	}

	/**
	 * Gets the 2 attributes of the 2 wilds from going up the parent path of a node
	 * @param n the node whos wild node's attrubutes need to be determined
	 * @return an array of size 2 with the attrubute values of the wild which lead to this node
	 */
  	private Object[] getRegionAndYearFromNode(Node n) {
	  Vector ret = new Vector(2,0);
	  do {
		  if(n.getNodeName().equals((String)wild.get(0)) || n.getNodeName().equals((String)wild.get(1))) {
			  //ret.add(n.getAttributes().getNamedItem("name").getNodeValue());
			  if(!n.hasAttributes()) {
				  ret.add(n.getNodeName());
			  } else {
				  ret.add(getOneAttrVal(n));
			  }
				  /*
			  } else if(!getOneAttrVal(n).equals("fillout=1")) {
			  	ret.add(getOneAttrVal(n));
			  } else {
			        ret.add(getOneAttrVal(n, 1));
			  }
			  */

		  }  
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
  	}

	/**
	 * not supported for a single table
	 */
	public void filterData(JFrame ParentFrame) {
		throw new UnsupportedOperationException();
	}

	/**
	 * can't execute a filter since it is not supported
	 */
	protected void doFilter(Vector possibleFilters){
		throw new UnsupportedOperationException();
	}

	/**
	 * Constructor used by multitablemodel which has already determined axis and data
	 * @param set1 column axis values
	 *        set1Name the node name which the column attrubutes come from
	 *        set2 row axis values
	 *        set2Name the node name which the row attrubutes come from
	 *        w3In a string which gives a better idea of where this thable comes from
	 *        dataIn the map which defines mapping of colKey;rowKey to data
	 *        docIn document of where the data comes from
	 */
	public NewDataTableModel(Collection set1, String set1Name, Collection set2, String set2Name, String w3In, Map dataIn, Document docIn,
			Documentation documentationIn) {
		documentation = documentationIn;
		w3 = w3In;
		title = w3;
		indCol = new Vector(set1);
		indCol.add(0,set1Name);
		indRow = new Vector(set2);
		if(indCol.contains("Units")) {
			for(Iterator i = indRow.iterator(); i.hasNext(); ) {
				String currRowName = (String)i.next();
				if(!dataIn.containsKey("Units;"+currRowName)) {
					i.remove();
				}
			}
		}
		data = dataIn;
		flipped = false;
		doc = docIn;
		ind1Name = set1Name;
		ind2Name = set2Name;
		documentationRenderer = getDocumentationRenderer();
	}

	/**
	 * Will be called by muli table model, switches the row and column headers, and names.
	 * Also sets a boolean so we know it has been flipped, since it makes a difference how we
	 * reference into the data map
	 * @param row not used here
	 *        col not used here
	 */
	public void flip(int row, int col) {
		Vector tempArr = indCol;
		indCol = indRow;
		indRow = tempArr;
		indRow.remove(0);
		String tempStr = ind1Name;
		ind1Name = ind2Name;
		ind2Name= tempStr;
		indCol.add(0, ind1Name);
		flipped = !flipped;
		fireTableStructureChanged();
		if(row >= 0 && col >= 0) {
			UndoManager undoManager = ((InterfaceMain)parentFrame).getUndoManager();
			undoManager.addEdit(new FlipUndoableEdit(this));
		}
		((InterfaceMain)parentFrame).refreshUndoRedo();
	}

	/**
	 * Returns the number of attributes for the column axis
	 * @return length of the column axis
	 */
	public int getColumnCount() {
		return indCol.size();
	}

	/**
	 * Returns the number of attributes for the row axis
	 * @return length of the row axis
	 */
	public int getRowCount() {
		return indRow.size();
	}

	protected Node getNodeAt(int row, int col) {
		if(col == 0) {
			return null;
		}
		return ((Node)data.get(getKey(row,col)));
	}

	/**
	 * Returns the value to be displayed in the table at a certain position
	 * @param row the row position in the table
	 *        col the col position in the table
	 * @return the data at the position requested
	 */
	public Object getValueAt(int row, int col) {
		// first column is for the row headers
		if(col ==0) {
			return indRow.get(row);
		}
		if(doc == null) {
			Object ret = data.get(getKey(row,col));
			if(ret == null) {
				return new Double(0.0);
			}
			return ret;
			/*
			XmlValue ret = ((XmlValue)data.get(getKey(row,col)));
			if(ret == null) {
				return "";
			}
			try {
				return ret.getNodeValue();
			} catch(Exception e) {
				e.printStackTrace();
				return null;
			}
			*/
		}
		Node ret = ((Node)data.get(getKey(row,col)));
		if(ret == null) {
			return "";
		}
		return ret.getNodeValue();
	}

	/**
	 * returns the attr value which defines the column passed in
	 * @param column an integer position to define which column
	 * @return the header value in the column index at the position passed in
	 */
	public String getColumnName(int column) {
		return (String)indCol.get(column);
	}

	/**
	 * Get the class for the data that will be in the specified class.  This
	 * will return String.class for the first and the last columns and
	 * Double.class for the rest.
	 * @param columnIndex The column being queried.
	 * @return Double.class for 1 &lt columnIndex &lt getColumnCount()-1 else String.class
	 */
	public Class getColumnClass(int columnIndex) {
		return (1 < columnIndex) && (columnIndex < getColumnCount()-1)? Double.class : String.class;
	}

	/**
	 * Used to tell which cells are editable, which are all but the first column, which is 
	 * reserved for row headers
	 * @param row the row position being queryed
	 *        col the column position being queryed
	 * @return true or false depeneding on if the cell is editable
	 */
	public boolean isCellEditable(int row, int col) {
		if(doc == null) {
			return false;
		}
		return col > 0;
	}

	/**
	 * Determines the key used to reference into the data map to get data
	 * @param row the row position of the data
	 *        col the column poition of the data
	 * @return the key in format key1;key2
	 */
	private String getKey (int row, int col) {
		// if it is filipped then the row should be the first key, and col is the second
		if(flipped) {
			return (String)indRow.get(row)+";"+(String)indCol.get(col);
		}
		return (String)indCol.get(col)+";"+(String)indRow.get(row);
	}

	/**
	 * Update the value in the cell specified. It there was data backing the cell
	 * previously, then it will just change the nodeValue, otherwise it will try to
	 * create a new node and add it to the tree, by attempting to analyze it's surrounding
	 * nodes, and it's row/column header values
	 * @param val new value the cell should be changed to
	 * @param row the row of the cell being edited
	 * @param col the col of the cell being edited
	 */
	public void setValueAt(Object val, int row, int col) {
		
		//TreeMap data = ((TreeMap)TreeMapVector.get( row / (indRow.size())));

		Node n = (Node)data.get(getKey(row,col));
		if( n != null ){
			n.setNodeValue(val.toString());
		}else{
			n = doc.createTextNode( val.toString() );
			Node updown = null;
			Node side = null;

			// Try to look in table for value in this column
			for(int i = 0; i < getRowCount() && ( updown =  (Node)data.get(getKey(i, col))) == null; ++i) {
			}
			// Try to look in this row to see if there is a value
			for(int i = 1; i < getColumnCount() && 
					( side = (Node)data.get(getKey(row, i))) == null; ++i) {
			}
			// If there weren't values in the same column and row won't be 
			// able to figure out the path down the tree to put the data
			if( updown == null || side == null ) {
				// throw some exception
				System.out.println("Couldn't gather enough info to create Node");
				JOptionPane.showMessageDialog(parentFrame, 
						"Couldn't gather enough information to \ncreate the data",
						"Set Value Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			ArrayList nodePath = new ArrayList();
			Node parent = ((Node)side.getParentNode());
			
			
			String headerone = ind1Name; // ex. region
			String headertwo = ind2Name; // ex. populationSGM

			String attributesLine = getKey( row, col );
			String[] attributesLineArray = attributesLine.split(";", 2);
			if(flipped) {
				String temp; //= attributesLineArray[0];
				/*
				attributesLineArray[0] = attributesLineArray[1];
				attributesLineArray[1] = temp;
				*/
				temp = headerone;
				headerone = headertwo;
				headertwo = temp;
			}

			/*
			StringTokenizer st = new StringTokenizer( attributesLineArray[ 1 ], "=", false);
			
			String attrFrom1 = st.nextToken();
			String attrTo1 = st.nextToken();
			
			st = new StringTokenizer( attributesLineArray[0], "=", false);
			String attrFrom2 = st.nextToken();
			String attrTo2 = st.nextToken();
			*/

			// attr names are not available here, but we can figure them out
			// when we reach the headone or headertwo(warning I am assuming the
			// first attr name is the correct one)
			String attrFrom1 = null;
			String attrTo1 = attributesLineArray[1];
			String attrFrom2 = null;
			String attrTo2 = attributesLineArray[0];

			// Work our way up the until we find the tag for corrent
			// column header which by the way the axis are chosen should 
			// always be higher in the path
			while( !parent.getNodeName().equals( headerone ) ) {
				nodePath.add(parent);
				parent = parent.getParentNode();
			}

			// figure out attrFrom1 by looking at parent
			NamedNodeMap nnm = parent.getAttributes();
			if(nnm.getLength() >0) {
				attrFrom1 = nnm.item(0).getNodeName();
			} else {
				attrFrom1 = null;
			}

			// Go down the path back to where the value should be
			// if there needs to be nodes created they will be using info 
			// from the row header, or the path info from the same row
			parent = parent.getParentNode();
			parent = checkPath(parent, headerone, attrFrom1, attrTo1);
			for(int i = nodePath.size()-1; i >= 0; --i) {
				Element temp = (Element)nodePath.get(i);
				if(temp.getNodeName().equals(headertwo)) {
					// figure out attrFrom2 by looking at parent
					nnm = temp.getAttributes();
					if(nnm.getLength() >0) {
						attrFrom2 = nnm.item(0).getNodeName();
					} else {
						attrFrom2 = null;
					}
					parent = checkPath(parent, headertwo, attrFrom2, attrTo2);
				} else {
					Node attrTemp = temp.getAttributes().item(0);
					if(attrTemp == null) {
						parent = checkPath(parent, temp.getNodeName(), null, null);
					} else {
						parent = checkPath(parent, temp.getNodeName(), attrTemp.getNodeName(), 
								attrTemp.getNodeValue());
					}
				}
			}

			parent.appendChild( n );
			data.put( getKey(row,col), n );
		}
		
		fireTableCellUpdated(row, col);

		// fireOffSomeListeners?

	}

	/** 
	 * Used to follow the path down a tree where parent is the current parent and want to 
	 * go under the node with the passed in node name and attributes will create the node
	 * if it does not exsit
	 * @param parent current node were are at in the path
	 * @param nodeName name of the node that we want to follow
	 * @param attrVal attribute value of the node that we want to follow
	 * @return the pointer to the node we wanted to follow
	 */
	private Node checkPath(Node parent, String nodeName, String attrKey, String attrVal) {
		NodeList nl = parent.getChildNodes();
		for(int i = 0; i < nl.getLength(); ++i) {
			Element temp = (Element)nl.item(i);
			if(temp.getNodeName().equals(nodeName) && attrVal == null) {
				return temp;
			} else if(temp.getNodeName().equals(nodeName) && temp.getAttribute(attrKey).equals(attrVal)) {
				return temp;
			}
		}
		Element newElement = doc.createElement(nodeName);
		if(attrKey != null) {
			newElement.setAttribute(attrKey, attrVal);
		}
		parent.appendChild(newElement);
		return newElement;
	}

	public JFreeChart createChart(int rowAt, int colAt) {
		// Start by creating an XYSeriesSet to contain the series.
		//XYSeriesCollection chartData = new XYSeriesCollection();
		org.jfree.data.xy.DefaultTableXYDataset chartData = new org.jfree.data.xy.DefaultTableXYDataset();
		// Loop through the rows and create a data series for each.
		for( int row = 0; row < getRowCount(); ++row ){
			// Row name is at element zero.
			String rowNameFull;
			if(chartLabelCol >= 0) {
				rowNameFull = (String)getValueAt(row, chartLabelCol);
			} else {
				rowNameFull = (String)getValueAt(row,0);
			}
			if(rowNameFull.equals("Total")) {
				continue;
			}
			
			// Split out the name attribute if it contains it.
			String rowName;
			if( rowNameFull.indexOf('=') != -1 ){
				rowName = rowNameFull.split("=")[ 1 ];
			}
			else {
				rowName = rowNameFull;
			}
			XYSeries currSeries = new XYSeries(rowName, false, false);
			// Skip column 1 because it contained the label.
			for( int col = 1; col < getColumnCount(); ++col ){
				String fullColumn = getColumnName(col);
				if(fullColumn.equals("Units")) {
					continue;
				}
				//double yValue = Double.parseDouble( (String)getValueAt(row, col) );
				Object gotVal = getValueAt(row, col);
				double yValue;
				if(gotVal == null) {
					yValue = 0;
				} else if(gotVal instanceof Double) {
					yValue = ((Double)gotVal ).doubleValue();
				} else {
					yValue = Double.parseDouble(gotVal.toString());
				}
				// Get the year part of it.
				if(fullColumn.indexOf('=') != -1) {
					fullColumn = fullColumn.split("=")[1];
				}
				int year = Integer.parseInt( fullColumn/*.split("=")[1]*/ );
				currSeries.add( year, yValue);
			}
			// Add the series to the set.
			chartData.addSeries(currSeries);
		}
		// Done adding series, create the chart.
		// Create the domain axis label.  // TODO: Improve naming.
		NumberAxis xAxis = new NumberAxis(/*ind2Name*/"Year");
		
		// Use the parent element name as the name of the axis.
		String appendUnits;
		if(units != null) {
			appendUnits = " ("+units+")";
		} else {
			appendUnits = "";
		}
		NumberAxis yAxis = new NumberAxis(ind2Name+appendUnits);
		
		// This turns off always including zero in the domain.
		xAxis.setAutoRangeIncludesZero(false);
		
		// This turns on automatic resizing of the domain..
		xAxis.setAutoRange(true);
		
		// This makes the X axis use integer tick units.
		xAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
		
		// This turns on automatic resizing of the range.
		yAxis.setAutoRange(true);
		
		// Create the plot.
		//XYPlot xyPlot = new XYPlot( chartData, xAxis, yAxis, new XYLineAndShapeRenderer());
		XYPlot xyPlot = new XYPlot( chartData, xAxis, yAxis, new org.jfree.chart.renderer.xy.StackedXYAreaRenderer());
		
		// Draw the zero line.
		//xyPlot.setZeroRangeBaselineVisible(true);
		
		// Create the chart.
		chart = new JFreeChart( xyPlot );
		
		// Create a title for the chart.
		TextTitle ttitle = new TextTitle(title);
		chart.setTitle(ttitle);
		//indCol.add("Chart");

		/*
		System.out.println("Setting label");
		BufferedImage chartImage = chart.createBufferedImage( 350, 350);
		if(icon == null) {
			icon = new ImageIcon(chartImage);
		} else {
			icon.setImage(chartImage);
		}
		*/

		return chart;
	}

	public void setColNameIndex(String name) {
		if(name != null) {
			for(int i = 0; i < getColumnCount(); ++i) {
				if(name.equals(getColumnName(i))) {
					chartLabelCol = i;
					return;
				}
			}
		}
		chartLabelCol = -1;
	}
	/*
	ImageIcon icon = null;
	public ImageIcon getChartImage() {
		return icon;
	}
	*/
  public void exportToExcel(HSSFSheet sheet, HSSFWorkbook wb, HSSFPatriarch dp) {
	  HSSFRow row = sheet.createRow(sheet.getLastRowNum()+1);
	  for(int i = 0; i < getColumnCount(); ++i) {
		  row.createCell((short)i).setCellValue(getColumnName(i));
	  }
	  for(int rowN = 0; rowN < getRowCount(); ++rowN) {
		  row = sheet.createRow(sheet.getLastRowNum()+1);
		  for(int col = 0; col < getColumnCount(); ++col) {
			  Object obj = sortedTable.getValueAt(rowN, col);
			  if(obj instanceof Double) {
				  row.createCell((short)col).setCellValue(((Double)obj).doubleValue());
			  } else {
				  row.createCell((short)col).setCellValue(getValueAt(rowN,col).toString());
			  }
		  }
	  }
	  try {
		  java.awt.image.BufferedImage chartImage = createChart(0,0).createBufferedImage(350,350);
		  // WARNING: This is a hack because of java some how looking to load some class that did
		  // not exist.  Instead of using the utilities which uses the Factory which uses the 
		  // reflextion which causes that mess I will use this encoder directly.
		  int where = wb.addPicture(new org.jfree.chart.encoders.SunJPEGEncoderAdapter().encode(chartImage), HSSFWorkbook.PICTURE_TYPE_JPEG);
		  dp.createPicture(new HSSFClientAnchor(0,0,50,50,(short)(getColumnCount()+1),
					  sheet.getLastRowNum()-getRowCount(),(short)(getColumnCount()+5),sheet.getLastRowNum()+1), where);
	  } catch(java.io.IOException ioe) {
		  ioe.printStackTrace();
	  }
  }
	public boolean equals(Object other) {
		if(other == this) {
			return true;
		} else if(!(other instanceof BaseTableModel) || other == null) {
			return false;
		} else if(other instanceof MultiTableModel) {
			return ((MultiTableModel)other).equals(this);
		} else {
			return false;
		}
	}
	public void annotate(int[] rows, int[] cols, Documentation documentation) {
		Vector<Node> selectedNodes = new Vector<Node>(rows.length*cols.length, 0);
		for(int i = 0; i < rows.length; ++i) {
			for(int j = 0; j < cols.length; ++j) {
				selectedNodes.add(getNodeAt(rows[i], cols[j]));
			}
		}
		documentation.getDocumentation(selectedNodes, rows, cols);
	}

}
