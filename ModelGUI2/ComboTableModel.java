import java.util.*;
import org.w3c.dom.*;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JScrollPane;
import javax.swing.*;
import javax.swing.tree.TreePath;
import org.w3c.dom.xpath.*;

public class ComboTableModel extends BaseTableModel{

	// new stuff
	Vector TreeMapVector = new Vector();
	Vector leftSideVector = new Vector();
	Vector leftHeaderVector;

	Vector indCol;
	Vector indRow;
	String ind1Name;
	String ind2Name;
	boolean flipped;

	Vector tables;

	public ComboTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString) {
		super(tp, doc, parentFrame, tableTypeString);
		leftHeaderVector = null;
		wild = chooseTableHeaders(tp, parentFrame);
		wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
		wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 0));
		activeRows = new Vector( leftSideVector.size() * indRow.size() );
		for(int i = 0; i < (leftSideVector.size() * indRow.size() ); i++) {
			activeRows.add(new Integer(i));
		}
		indCol.add(0, ind1Name);
	}
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
		doFilter( new Vector(tableFilterMaps.keySet()) );
		fireTableStructureChanged();
				//((NewDataTableModel)((JTable)((JScrollPane)getValueAt(row, col)).getViewport().getView()).getModel()).flip(row, col);
	}

	protected void buildTable(XPathExpression xpe) {
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  Object[] regionAndYear;
	  TreeSet regions = new TreeSet();
	  TreeSet years = new TreeSet();
	  tableFilterMaps = new LinkedHashMap();
	  TreeMap dataTree = new TreeMap();
	  while ((tempNode = res.iterateNext()) != null) {
		regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode(), tableFilterMaps);
		regions.add(regionAndYear[0]);
		years.add(regionAndYear[1]);
		addToDataTree(tempNode, dataTree).put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
	  }
	  recAddTables(dataTree, null, regions, years, "");
	  indCol = new Vector( regions );
	  indRow = new Vector( years );
	  ind1Name = (String)wild.get(0);
	  ind2Name = (String)wild.get(1);
	}
	private Object[] getRegionAndYearFromNode(Node n, Map filterMaps) {
	  Vector ret = new Vector(2,0);
	  do {
		  if(n.getNodeName().equals((String)wild.get(0)) || n.getNodeName().equals((String)wild.get(1))) {
			  //ret.add(n.getAttributes().getNamedItem("name").getNodeValue());
			  if(!n.hasAttributes()) {
				  ret.add(n.getNodeName());
			  } else if(!getOneAttrVal(n).equals("fillout=1")) {
				ret.add(getOneAttrVal(n));
			  } else {
					ret.add(getOneAttrVal(n, 1));
			  }

		  } else if(n.hasAttributes()) {
			  HashMap tempFilter;
				  if (filterMaps.containsKey(n.getNodeName())) {
							  tempFilter = (HashMap)filterMaps.get(n.getNodeName());
						  } else {
								  tempFilter = new HashMap();
						  }
			  String attr = getOneAttrVal(n);
			  if (!tempFilter.containsKey(attr)) {
							tempFilter.put(attr, new Boolean(true));
							filterMaps.put(n.getNodeName(), tempFilter);
			  }
		  }
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
	}
  private TreeMap addToDataTree(Node currNode, TreeMap dataTree) {
	  if (currNode.getNodeType() == Node.DOCUMENT_NODE) {
		  return dataTree;
	  }
	  TreeMap tempMap = addToDataTree(currNode.getParentNode(), dataTree);
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && currNode.getNodeName().equals("subsector") ) {
		  return tempMap;
	  }
	  if(currNode.hasAttributes() && !currNode.getNodeName().equals((String)wild.get(0)) && !currNode.getNodeName().equals((String)wild.get(1))) {
		String attr = currNode.getNodeName()+"@"+getOneAttrVal(currNode);
		if(!tempMap.containsKey(attr)) {
			tempMap.put(attr, new TreeMap());
		}
		return (TreeMap)tempMap.get(attr);
	  }
	  return tempMap;
  }

  private void recAddTables(TreeMap dataTree, Map.Entry parent, TreeSet regions, TreeSet years, String title) {
	Iterator it = dataTree.entrySet().iterator();
	while(it.hasNext()) {	
		Map.Entry me = (Map.Entry)it.next();
		if(me.getValue() instanceof Node) {
			TreeMapVector.add( (TreeMap)parent.getValue() );
			
			// create a left side 2d vector, add it to LeftSideVector
			
			String lineToParse = title+'/';
			
			// example:		/populationSGMRate@year=1985/gender:type=female/
	
			// get rid of begin and end '/'
			lineToParse = lineToParse.substring( 1, lineToParse.length()-1 );
			
			StringTokenizer st = new StringTokenizer( lineToParse, "/", false);
			int numberOfThem = st.countTokens();
			
			Vector onerow = new Vector( numberOfThem );
			Vector tempVector = new Vector();
			while( st.hasMoreTokens() ){
				//onerow = new Vector( numberOfThem );
				String allNodeInfo = st.nextToken(); // first one
				// 		populationSGMRate@year=1985
				StringTokenizer innerSt = new StringTokenizer( allNodeInfo, "@", false);
				if( innerSt.countTokens() != 2 ){
					System.out.println("BIG PROBLEM, COUNT TOKENS ISN'T 2!!!!!!!!!!");
					return;
				}
				String firstHalf = innerSt.nextToken(); //	populationSGMRate
				if(leftHeaderVector == null){
					tempVector.add( firstHalf );
				}
				String secHalf = innerSt.nextToken(); //	year=1985
				onerow.add( secHalf );
			}
			if(leftHeaderVector == null) {
				leftHeaderVector = tempVector;
			}
			if( ! onerow.isEmpty() ){
				leftSideVector.add( onerow );
			}
			return;
		}else{
			recAddTables((TreeMap)me.getValue(), me, regions, years, title+'/'+(String)me.getKey());
		}
		/*

			NewDataTableModel tM = new NewDataTableModel(regions, (String)wild.get(0), years, (String)wild.get(1), title+'/'+(String)parent.getKey(), (TreeMap)parent.getValue(), doc); 
			JTable jTable = new JTable(tM);

			jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

			javax.swing.table.TableColumn col;
			Iterator i = regions.iterator();
			int j = 0;
			while(i.hasNext()) {
				col = jTable.getColumnModel().getColumn(j);
				col.setPreferredWidth(((String)i.next()).length()*5+30);
				j++;
			}
			CopyPaste copyPaste = new CopyPaste( jTable );
			JScrollPane tableView = new JScrollPane(jTable);
			if(tables == null) {
				tables = new Vector();
			}
			tables.add(title+"/");
			tables.add(tableView);
			return;
		} else {
			recAddTables((TreeMap)me.getValue(), me, regions, years, title+'/'+(String)me.getKey());
		}*/
	}
  }
  
  private String getKey (int row, int col) {
	  if(flipped) {
		  return (String)indRow.get(row % (indRow.size()))+";"+(String)indCol.get(col - leftHeaderVector.size());
	  }
	  return (String)indCol.get(col- leftHeaderVector.size())+";"+(String)indRow.get(row % (indRow.size()));
  }

  
	public int getColumnCount() {
		return leftHeaderVector.size() + indCol.size();
	}
	public int getRowCount() {
		return activeRows.size();
	}
	public Object getValueAt(int row, int col) {
		try{
			//System.out.println( "row and col are " + row + " " + col );
			if( col < leftHeaderVector.size() ){
				//System.out.println( "part1 " + ((Integer)activeLeft.get( row )).intValue() / (indRow.size()));
				//System.out.println("part2 "+((Vector)leftSideVector.get( ((Integer)activeLeft.get( row )).intValue()/ (indRow.size()) )).get(col));
				return ((Vector)leftSideVector.get( ((Integer)activeRows.get( row )).intValue() / (indRow.size()))).get( col );
			}else if( col == leftHeaderVector.size() ){
				return indRow.get( ((Integer)activeRows.get( row )).intValue() % (indRow.size()) );
			}else{
				//System.out.println( "2ndpart1 " + ((Integer)activeLeft.get( row )).intValue() / (indRow.size()));
				//System.out.println("2ndpart2 "+((TreeMap)TreeMapVector.get( ((Integer)activeRows.get( row )).intValue() / (indRow.size()))).get( getKey( row, col ) ));
				return new Double(((Node)((TreeMap)TreeMapVector.get( ((Integer)activeRows.get( row )).intValue() / (indRow.size()))).get( getKey( row, col ) )).getNodeValue());
			}
		} catch(NullPointerException e) {
			return "";
		} catch(NumberFormatException nf) { // if the data is not numbers
			return ((Node)((TreeMap)TreeMapVector.get( ((Integer)activeRows.get( row )).intValue() / (indRow.size()))).get( getKey( row, col ) )).getNodeValue();
		}
	}
	public String getColumnName(int col) {
		if( col < leftHeaderVector.size() ){
			return (String)leftHeaderVector.get( col );
		}else{
			return (String)indCol.get( col - leftHeaderVector.size() );
		}
	}
	public boolean isCellEditable(int row, int col) {
		if( col <= leftHeaderVector.size() ){
			return false;
		}else{
			return true;
		}
	}

	protected void doFilter(Vector possibleFilters) {
			activeRows = new Vector();
			for (int i = 0; i < (leftSideVector.size() * indRow.size()); i++) {
				activeRows.addElement(new Integer(i));
			}
			Integer rowPos = new Integer(-1);

			for (int i = 0; i < possibleFilters.size(); i++) {
				if (((String)possibleFilters.get(i)).equals("")) {
					continue;
				}
				currKeys = (String[])((Map)tableFilterMaps.get((String)possibleFilters.get(i))).keySet().toArray(new String[0]);
				//for (Iterator it = activeRows.iterator(); it.hasNext(); rowPos = (Integer)it.next()) {
				Iterator it = activeRows.iterator();
				while (it.hasNext()) {
					rowPos = (Integer)it.next();
					for (int j = 0; j < currKeys.length; j++) {
						if (!((Boolean)((Map)tableFilterMaps.get((String)possibleFilters.get(i))).get(currKeys[j])).booleanValue() ){
							if (((String)((Vector)leftSideVector.get( rowPos.intValue() / (indRow.size()) )).get( possibleFilters.size()-i-1 )).equals(currKeys[j])){
								it.remove();
								break;
	
							}
						}
					}
				}
			}
	}
	
	public void setValueAt(Object val, int row, int col) {
		
		TreeMap data = ((TreeMap)TreeMapVector.get( row / (indRow.size())));

		Node n = (Node)data.get(getKey(row,col));
		if( n != null ){
			n.setNodeValue(val.toString());
		}else{
			n = doc.createTextNode( val.toString() );
			Node updown = null;
			Node side = null;
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
			}
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
					index = nodepath.indexOf( parent ); // gives me '5'
						
				}
				parent = ((Node)parent.getParentNode());
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
								stopsplitcount = true;
							}	
						}
					}
				}
				splitcount++;
			}
			
			for(int i=0; i< nodepath.size(); i++){
			}
			
			index--;
			
			//curr = (Node)nodepath.get( index );
			if ( index >= 0 ){
				for(int theRest = index; theRest >= 0; theRest-- ){
					Node pathNext = ((Node)nodepath.get( theRest ));
					
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
							if (attrs1.getLength() == attrs2.getLength()) {
								if ( attrs1.getLength() == 0 ){
									curr = child;
									getOutOfLoop = true;
								}else{

									for (int i = 0; i < attrs1.getLength(); i++) {
										temp = attrs1.item(i).getNodeName();
										if (eChild.getAttribute(temp).equals(ePathNext.getAttribute(temp))) {
											// found the node!
											curr = child; // move down the list
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
					}
				}
			}// now index should == nodepath.size() - 1
			
			
			curr.appendChild( n );
			data.put( getKey(row,col), n );
		}
		
		fireTableCellUpdated(row, col);

		// fireOffSomeListeners?

	}
}
