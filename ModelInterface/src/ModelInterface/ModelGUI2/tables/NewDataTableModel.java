package ModelInterface.ModelGUI2.tables;

import ModelInterface.ModelGUI2.DOMmodel;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.tree.TreePath;

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
	TreeMap data;
	boolean flipped;
	String w3;
	JFreeChart chart;
	//Document doc;

	/**
	 * Constructor initializes data members, and calls buildTable to initialize data, and filterMaps
	 * @param tp the Tree Path which was selected from the tree, needed to build table
	 *        doc needed to run the XPath query against
	 *        parentFrame needed to create dialogs
	 *        tableTypeString to be able to display the type of table this is
	 */
	public NewDataTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString) {
		super(tp, doc, parentFrame, tableTypeString);
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
	public NewDataTableModel(Collection set1, String set1Name, Collection set2, String set2Name, String w3In, TreeMap dataIn, Document docIn) {
		w3 = w3In;
		title = w3;
		indCol = new Vector(set1);
		indCol.add(0,set1Name);
		indRow = new Vector(set2);
		data = dataIn;
		flipped = false;
		doc = docIn;
		ind1Name = set1Name;
		ind2Name = set2Name;
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

	/* not used anymore
	public Node getNodeAt(int row, int col) {
		if(col == 0) {
			return null;
		}
		return ((Node)data.get(getKey(row,col)));
	}
	*/

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
			Double ret = (Double)data.get(getKey(row,col));
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
		if(!isCellEditable(row, col)) {
			return;
		}
		/*
		Object nO = data.get(getKey(row,col));
		if(nO instanceof Node) {
			Node n = (Node)nO;
			*/
		Node n = (Node)data.get(getKey(row,col));
		if( n != null ){
			n.setNodeValue(val.toString());
		}else{
			n = doc.createTextNode( val.toString() );
			Node updown = null;
			Node side = null;
			
			// time to search for 'updown' and 'side'
			int theRow = -1;
			int theCol = -1;
			for(int r=0; r< getRowCount(); r++ ){
				if( r != row && ( (Node)data.get(getKey(r, col)) != null )){
					theRow = r;
					break;
				}
			}
			for(int c=1; c< getColumnCount(); c++ ){
				if( c != col && ( (Node)data.get(getKey(row, c)) != null )){
					theCol = c;
					break;
				}
			}
			if( theRow == -1 || theCol == -1 ){
				System.out.println("not enough info to do the null replacement thing");
				JOptionPane.showMessageDialog(parentFrame, "Not enough information to create node on the fly", "Rewrite nonexistent node Warning", JOptionPane.WARNING_MESSAGE);
				return;
			}
			updown = (Node)data.get(getKey(theRow, col));
			side = (Node)data.get(getKey(row, theCol));
			if( updown == null || side == null){
				System.out.println("BIG TROUBLE NOW! THESE SHOULD NOTTTT BE NULL");
			}
			
			/*
			if( row > 0 ){
				updown = (Node)data.get(getKey(row-1, col));
				if ( col > 0 ){
					side = (Node)data.get(getKey(row, col-1));
				}else{ // col == 0
					side = (Node)data.get(getKey(row, col+1));
				}
			}else{ // row == 0
				updown = (Node)data.get(getKey(row+1, col));
				if ( col > 0 ){
					side = (Node)data.get(getKey(row, col-1));
				}else{ // col == 0
					side = (Node)data.get(getKey(row, col+1));
				}
			}*/
			
			ArrayList nodepath = new ArrayList();
			Node parent = ((Node)updown.getParentNode());
			

			
			String headerone = ind1Name; // ex. region
			String headertwo = ind2Name; // ex. populationSGM
			
			String attributesLine = getKey( row, col );
			String[] attributesLineArray = attributesLine.split(";", 2);
			if(flipped) {
				String temp = attributesLineArray[0];
				attributesLineArray[0] = attributesLineArray[1];
				attributesLineArray[1] = temp;
			}

			StringTokenizer st = new StringTokenizer( attributesLineArray[ 0 ], "=", false);
			
			String attrFrom1 = st.nextToken();
			String attrTo1 = st.nextToken();
			
			st = new StringTokenizer( attributesLineArray[1], "=", false);
			String attrFrom2 = st.nextToken();
			String attrTo2 = st.nextToken();
			
			int index = 0;
			boolean stoplooking = false;
			while( parent != null ){
				nodepath.add( parent );
				if ( !stoplooking && (parent.getNodeName().equals( headerone ) )){ // or headertwo
					//System.out.println("found headtwo! parent is " + parent.getNodeName());
					index = nodepath.indexOf( parent ); // gives me '5'
					//System.out.println("index is ... " + index + " found it!");
						
				}
				parent = ((Node)parent.getParentNode());
				//System.out.println("looking for parent");
			}
			
			// index is 5, i want to split on 5+1 = 6
			
			Node parentOfSplit = ((Node)nodepath.get( index + 1 ));
			Node curr = parentOfSplit; // set to default for now..
			
			// locate precise child... of 'world', looking for region name = USA
			
			NodeList splitlist = parentOfSplit.getChildNodes();
			int splitcount = 0;
			boolean stopsplitcount = false;
			while( splitcount < splitlist.getLength() && !stopsplitcount ){
				Node onechild = ((Node)splitlist.item( splitcount ));
				if (onechild.getNodeName().equals( headerone )){
						
					Element elemChild = (Element)onechild;
					NamedNodeMap attrSplit = elemChild.getAttributes();
					String temporary;

					for (int i = 0; i < attrSplit.getLength(); i++) {
						if( attrSplit.item(i).getNodeName().equals( attrFrom2 )){
							if( attrSplit.item(i).getNodeValue().equals( attrTo2 )){
								curr = onechild; // move down the list
								//System.out.println("FOUND IT!");
								//System.out.println("curr is " + curr.getNodeName() );
								stopsplitcount = true;
							}	
						}
					}
				}
				splitcount++;
			}
			
			/*
			System.out.println("node path is ");
			for(int i=0; i< nodepath.size(); i++){
				System.out.println(i + " " + ((Node)nodepath.get(i)).getNodeName());	
			}
			*/
			
			index--;
			//System.out.println("index is " + index);
			
			//curr = (Node)nodepath.get( index );
			//System.out.println("curr is " + curr.getNodeName());
			if ( index >= 0 ){
				//System.out.println("in loop, index it " + index);
				for(int theRest = index; theRest >= 0; theRest-- ){
					//System.out.println("in for loop, theRest is " + theRest);
					Node pathNext = ((Node)nodepath.get( theRest ));
					//System.out.println("pathnext's is " + pathNext.getNodeName() );
					
					NodeList children = curr.getChildNodes(); // either find it or create it
					int counter = 0;					
					boolean getOutOfLoop = false;
					while( counter < children.getLength() && !getOutOfLoop ){
						Node child = ((Node)children.item( counter ));
						if( child.getNodeName().equals( pathNext.getNodeName() ) ){

							Element eChild = (Element)child;
							Element ePathNext = (Element)pathNext;
							// go through all the attributes, make sure have the same ammount and the have the same values
							NamedNodeMap attrs1 = eChild.getAttributes();
							NamedNodeMap attrs2 = ePathNext.getAttributes();
							String temp;
							//System.out.println("are they not the right size? " + attrs1.getLength() + " " + attrs2.getLength());
							if (attrs1.getLength() == attrs2.getLength()) {
								if ( attrs1.getLength() == 0 ){
									curr = child;
									//System.out.println("found inside the node!");	
									getOutOfLoop = true;
								}else{

									for (int i = 0; i < attrs1.getLength(); i++) {
										temp = attrs1.item(i).getNodeName();
										if (eChild.getAttribute(temp).equals(ePathNext.getAttribute(temp))) {
											// found the node!
											curr = child; // move down the list
											//System.out.println("inside found the node!");
											//System.out.println("curr is " + curr.getNodeName()+ " moving down the list!");
											getOutOfLoop = true;
										}
									}
								}
							}
						}					
						counter++;	
					}
					if( getOutOfLoop == false ){ // didn't find it, create it
						Node createdNode = pathNext.cloneNode( false ); // clone the node and add it
						curr.appendChild( createdNode );
						curr = createdNode;
						//System.out.println("didn't find it, moving curr");
						//System.out.println("createdNode's name is " + createdNode.getNodeName());
					}
				}
			}// now index should == nodepath.size() - 1
			
			//System.out.println("end, curr is ... " + curr.getNodeName());
			
			curr.appendChild( n );
			data.put( getKey(row,col), n );
			//System.out.println("just appended the child");
			//System.out.println("curr is " + curr.getNodeName());
			//System.out.println(" n is " + n.getNodeName());
		}
		/*
		} else if( nO instanceof XmlValue ) {
			XmlValue n = (XmlValue)nO;
			if(n != null) {
				FileChooserDemo.xmlDB.setValue(n, (String)val);
				createChart(0,0);
				/*
				   System.out.println(XmlValue.TEXT_NODE+"");
				   XmlValue temp = new XmlValue(XmlValue.TEXT_NODE, (String)val);
				   System.out.println(temp.getType()+" type");
				   XmlValue.setValue(n, temp);
				//data.put(getKey(row,col), temp);
				System.out.println("Tried to set it's value");
			}
		}
		*/
		fireTableCellUpdated(row, col);
	}

	public JFreeChart createChart(int rowAt, int colAt) {
		// Start by creating an XYSeriesSet to contain the series.
		//XYSeriesCollection chartData = new XYSeriesCollection();
		org.jfree.data.xy.DefaultTableXYDataset chartData = new org.jfree.data.xy.DefaultTableXYDataset();
		// Loop through the rows and create a data series for each.
		for( int row = 0; row < getRowCount(); ++row ){
			// Row name is at element zero.
			String rowNameFull = (String)getValueAt(row,0);
			
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
				String fullColumn = getColumnName(col);
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
		NumberAxis yAxis = new NumberAxis( ind2Name);
		
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
			  Object obj = getValueAt(rowN, col);
			  if(obj instanceof Double) {
				  row.createCell((short)col).setCellValue(((Double)obj).doubleValue());
			  } else {
				  row.createCell((short)col).setCellValue(getValueAt(rowN,col).toString());
			  }
		  }
	  }
	  try {
		  java.awt.image.BufferedImage chartImage = createChart(0,0).createBufferedImage(350,350);
		  int where = wb.addPicture(org.jfree.chart.ChartUtilities.encodeAsPNG(chartImage), HSSFWorkbook.PICTURE_TYPE_PNG);
		  System.out.println("Added to "+where);
		  dp.createPicture(new HSSFClientAnchor(0,0,50,50,(short)(getColumnCount()+1),
					  sheet.getLastRowNum()-getRowCount(),(short)(getColumnCount()+5),sheet.getLastRowNum()+1), where);
	  } catch(java.io.IOException ioe) {
		  ioe.printStackTrace();
	  }
  }
}
