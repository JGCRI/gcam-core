import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.*;
import javax.swing.JFrame;
//import javax.swing.*;
//import java.awt.*;
//import java.awt.event.*;
import javax.swing.tree.TreePath;
import org.apache.xpath.domapi.*;
import org.w3c.dom.xpath.*;

public class NewDataTableModel extends BaseTableModel{
	Vector indCol;
	Vector indRow;
	String ind1Name;
	String ind2Name;
	TreeMap data;
	boolean flipped;
	String w3;
	//Document doc;

	public NewDataTableModel(TreePath tp, Document doc, JFrame parentFrame) {
		super(tp, doc, parentFrame);
		indCol = new Vector();
		indRow = new Vector();
		data = new TreeMap();
		w3 = "";
		wild = chooseTableHeaders(tp, parentFrame);
	        wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
	        wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 1));
		indCol.add(0,w3 /*set2Name*/);
		ind1Name = (String)wild.get(0);
		ind2Name = (String)wild.get(1);
		flipped = false;
	}
	protected void buildTable(XPathExpression xpe) {
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  TreeSet col = new TreeSet();
	  TreeSet row = new TreeSet();
	  Node tempNode;
	  Object[] regionAndYear;
	  while ((tempNode = res.iterateNext()) != null) {
		regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode());
		col.add(regionAndYear[0]);
		row.add(regionAndYear[1]);
		data.put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
	  }
	  indCol = new Vector(col);
	  indRow = new Vector(row);
	}
  	private Object[] getRegionAndYearFromNode(Node n) {
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

		  }  
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
  	}
	protected void filterData(JFrame ParentFrame) {
		throw new UnsupportedOperationException();
	}
	protected void doFilter(Vector possibleFilters){
		throw new UnsupportedOperationException();
	}
	public NewDataTableModel(Collection set1, String set1Name, Collection set2, String set2Name, String w3In, TreeMap dataIn, Document docIn) {
		w3 = w3In;
		indCol = new Vector(set1);
		indCol.add(0,w3 /*set2Name*/);
		indRow = new Vector(set2);
		data = dataIn;
		flipped = false;
		doc = docIn;
		ind1Name = set1Name;
		ind2Name = set2Name;
	}

	public void flip(int row, int col) {
		Vector tempArr = indCol;
		indCol = indRow;
		indRow = tempArr;
		indRow.remove(0);
		String tempStr = ind1Name;
		ind1Name = ind2Name;
		ind2Name= tempStr;
		indCol.add(0, w3 /*ind2Name*/);
		flipped = !flipped;
		fireTableStructureChanged();
	}

	public int getColumnCount() {
		return indCol.size();
	}

	public int getRowCount() {
		return indRow.size();
	}

	public Node getNodeAt(int row, int col) {
		if(col == 0) {
			return null;
		}
		return ((Node)data.get(getKey(row,col)));
	}
	public Object getValueAt(int row, int col) {
		if(col ==0) {
			return indRow.get(row);
		}
		Node ret = ((Node)data.get(getKey(row,col)));
		if(ret == null) {
			return "";
		}
		return ret.getNodeValue();
	}

	public String getColumnName(int column) {
		return (String)indCol.get(column);
	}

	public boolean isCellEditable(int row, int col) {
		return col > 0;
	}

	private String getKey (int row, int col) {
		if(flipped) {
			return (String)indRow.get(row)+";"+(String)indCol.get(col);
		}
		return (String)indCol.get(col)+";"+(String)indRow.get(row);
	}

	public void setValueAt(Object val, int row, int col) {
		if(!isCellEditable(row, col)) {
			return;
		}
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
		fireTableCellUpdated(row, col);
	}
}
