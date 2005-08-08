/*!
*	\file DOMTreeBuilder.java
*	\ingroup CAIM
*	\brief The DOMTreeBuilder class to build a DOM tree from CSV data.
*	\author Pralit Patel
*	\author Kathrine Chung
*	\date $Date$
*	\version $Revision$
*/
//package ModelGUI2;
package ModelInterface.ModelGUI2;
import java.io.FileWriter;
import java.util.*;
import org.w3c.dom.*;
import org.w3c.dom.bootstrap.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;

/*!
*	\ingroup CAIM
*       \brief Converts CSV formated tables to XML.
*
*	Class creates a DOM document from CSV, adds to the tree using specified header info,
*	and data array. Can also output the resulting XML to a specified file.
*
*	\author Pralit Patel
*	\author Katherine Chung
*	\warning User need to have the DOM level 3 implementation installed, currently BETA for java 1.5.0 and Xerces 2.6.2
*/

public class DOMTreeBuilder {

	private Document doc; //!< Keeps the instance of the DOM document being used to store the XML tree.
	private Headers head; //!< Has the parsed header info necessary to build nodes.
	private ArrayList dataArr; //!< Current line of data being processed.
	private Node rootElement; //!< Holds the pointer to the root of the DOM tree
	private String docName; //!< Holds the tagName of the root
	private HashMap lookUpMap;
	private boolean buildMap;

	/*! \brief Default constructor.
	*
	*   Initializes the document by loading up the DOMImplementation for XML 3.0
	*
	*  \warning User need to have the DOM level 3 implementation installed, currently BETA for java 1.5.0 and Xerces 2.6.2
	*/
	public DOMTreeBuilder() {
		docName = "scenario";
		try {
			System.setProperty(DOMImplementationRegistry.PROPERTY, "org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry reg = DOMImplementationRegistry.newInstance();
			DOMImplementation impl = reg.getDOMImplementation( "XML 3.0" );
		    	if (impl == null) {
		    		System.out.println("Could not find a DOM3 Load-Save compliant parser.");
			        return;
			}
			// Create the document
			DocumentType svgDOCTYPE = impl.createDocumentType(docName, "","");
			//doc = impl.createDocument("http://www.w3.org/2000/svg", docName, svgDOCTYPE);
			doc = impl.createDocument("", docName, svgDOCTYPE);
			rootElement = doc.getDocumentElement();
		} catch (Exception e) {
			System.err.println(e);
		}
		lookUpMap = new HashMap();
		buildMap = false;
	}

	/* \brief Set the header.
	*
	*  Setting the header specified the format of the data.
	*
	*/
	public void setHeader(String headerIn) throws Exception {
		if(headerIn.matches(".*MAP,.*")) {
			buildMap = true;
		} else {
			buildMap = false;
		}
		head = new Headers(headerIn.split(","));
	}

	/*! \brief Adds the current data to the Tree
	*
	*   calls makeTree using the data passed in and the current Headers set to
	*   specify how to create the Nodes.
	*
	*   \warning user must set a header before calling this function.
	*/
	public void addToTree (ArrayList data) {
		if (head == null && !buildMap) {
			System.out.println("Warning, no header set, skipping data");
			return;
		}
		dataArr = data;
		if (buildMap) {
			//System.out.println("Processing map");
			addToMap();
		} else {
			makeTree(rootElement, docName);
		}
	}

	/* \brief Outputs the Tree in XML format to filename.
	*
	*  Uses standard XML serializers to process and output the DOM tree.
	*
	*/
	public void outputTree(String filename) {
		// specify output formating properties
		OutputFormat format = new OutputFormat(doc);
		format.setEncoding("UTF-8");
		format.setLineSeparator("\n");
		format.setIndenting(true);
		format.setIndent(3);
		format.setLineWidth(0);
		format.setPreserveSpace(false);
		format.setOmitDocumentType(true);

		// create the searlizer and have it print the document
		try {
			XMLSerializer serializer = new XMLSerializer (new FileWriter(filename), format);
			serializer.asDOMSerializer();
			serializer.serialize(doc);
		} catch (java.io.IOException e) {
			System.err.println("Error outputing tree: "+e);
		}
	}

	public Document getDoc() {
		finalize(doc.getDocumentElement());
		return doc;
	}

	private void addToMap() {
		HashMap tempMap;
		//System.out.println(head.getNumCols());
		if(lookUpMap.containsKey(head.getParentHeader(0))) {
			tempMap = (HashMap)lookUpMap.get(head.getParentHeader(0));
		} else {
			tempMap = new HashMap();
		}
		if (dataArr.size() == 3) {
			tempMap.put((String)dataArr.get(0) + "&&" + (String)dataArr.get(1),dataArr.get(2));
		} else {
			tempMap.put(dataArr.get(0),dataArr.get(1));
		}
		for(int i = 0; i < head.getNumCols(); i++) {
			//System.out.println("Adding: "+head.getParentHeader(i)+" -> "+dataArr.get(0)+" -> "+dataArr.get(1));
			lookUpMap.put(head.getParentHeader(i),tempMap);
		}
	}

	private String convertData(String elementName, String dataRead, Element parent, String attrName) {
		Object ret = lookUpMap.get(elementName);
		if(ret == null) {
			return dataRead;
		}
		ret = ((HashMap)ret).get(attrName+dataRead);
		if(ret == null) {
			ret = ((HashMap)lookUpMap.get(elementName)).get(parent.getAttribute(attrName)+"&&"+attrName+dataRead);
			if (ret == null ) {
				return dataRead;
			}
		}
		return (String)ret;
	}

	/*! \brief Compare to Individual Elements of a DOM tree.
	*
	*   Elements are considered to be the same if they both have the same tag name,
	*   same attrubutes, and same TEXT data if any.
	*
	*   \return Returns true if the elements are the same, false otherwise.
	*/
	public static boolean compareHelper(Element e1, Element e2){ // helper for compare function
		// first make sure tag names are the same
		if( !((e1.getTagName()).equals(e2.getTagName()) ) ){
			return false;
		}

		// go through all the attributes, make sure have the same ammount and the have the same values
		NamedNodeMap attrs1 = e1.getAttributes();
		NamedNodeMap attrs2 = e2.getAttributes();
		String temp;
		if (attrs1.getLength() != attrs2.getLength()) {
			return false;
		}
		for (int i = 0; i < attrs1.getLength(); i++) {
			temp = attrs1.item(i).getNodeName();
			if (!(e1.getAttribute(temp).equals(e2.getAttribute(temp)))) {
				return false;
			}
		}
		// go through the children and look for a TEXT_NODE, and check to make sure the data in them are
		// the same
		NodeList child1 = e1.getChildNodes();
		NodeList child2 = e2.getChildNodes();
		int textchild1 = -1;
		int textchild2 = -1;
		for(int i=0; i<child1.getLength(); i++){
			if(child1.item(i).getNodeType() == Element.TEXT_NODE)
				textchild1 = i;
		}
		for(int i=0; i<child2.getLength(); i++){
			if(child2.item(i).getNodeType() == Element.TEXT_NODE)
				textchild2 = i;
		}
		if((textchild1 == -1) && (textchild2 == -1)){
			return true;
		}
		return true;
		/*
		if(textchild1 != -1 && textchild2 != -1 ){
			if(!child1.item(textchild1).getNodeValue().equals (child2.item(textchild2).getNodeValue())) {
				return false;
			}
			else{
				return true;
			}
		}else{
			return false;
		}
		*/
	}

	/*! \brief Compare all of the imediate children of e1 to e2.
	*
	*   Compares each element and if any of the children are the same as e2
	*   it shall be returned.
	*
	*   \return Returns the Node which is the same as e2 and already exists in the tree, or null.
	*/
	public Node compare( Element e1, Element e2 ){
		// get all the immediate children of e1 and compare them to e2
		NodeList list1 = e1.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() != Element.TEXT_NODE){
				if(compareHelper((Element)list1.item(i), e2)){
					return list1.item(i);
				}
			}
		}
		return null;
	}

	private Node hasTextData(Node curr) {
		NodeList list1 = curr.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() == Element.TEXT_NODE){
				return list1.item(i);
			}
		}
		return null;
	}

	/*! \brief Creates the DOM tree.
	*
	*   Starts from the root of the tree, and creates nodes in the appropriate positions,
	*   determined through the headers, fills nodes with the correct current data.
	*
	*/
	private void makeTree( Node parent, String name) {
                String chName;
		int retInt;
                ArrayList children = head.getIndecesWhereParentHeaderIs(name);
                Element tempNode;
                Text tempText = null;
		Node retNode;
		String attrStr;
		Object retVal;
		Iterator it = children.iterator();
		int currPos;
		// loop through all the headers that are the immediate child of name
                //for (int i = 0; i < children.size(); i++) {
		//System.out.println(name+" has child# "+children.size());
		while(it.hasNext()) {
			currPos = ((Integer)it.next()).intValue();
                        chName = head.getChildHeader(currPos);
			if(chName.startsWith("#text")) {
				parent.appendChild(doc.createTextNode(chName.substring(6)));
				return;
			}
			//System.out.println(name+" --- "+chName+" --- "+head.checkChildStackFront());
			// this indicates that the child with specify which parent it belongs under
			if ( chName.startsWith( "*" ) && head.checkChildStackFront() != currPos) {
				makeTreeHelper( parent, chName.substring( 1, chName.length() ) );
			}
			else if (!head.getParentHeader(currPos).startsWith("{") && head.getParentHeader(currPos).matches(".*\\..*") && head.checkChildStackFront() != currPos) {
				chName = head.getParentHeader(currPos);
				//System.out.println("ADDING FROM STACK SINGLE");
				head.addToChildStack(currPos);
				chName = '@' +chName.substring(0,chName.indexOf("/"));
				//System.out.println("About to use "+chName);
				makeTree(parent, chName);
				//System.out.println("Now REMOVING FROM STACK SINGLE");
				head.popChildStack();
				// check to see if this node exists already so as to not duplicate it, and
				// recurse using this child as the new parent
				/*
                        	if ((retNode = compare((Element)parent, tempNode)) == null) {
                                	parent.appendChild(tempNode);
                                	makeTree(tempNode, chName);
                        	} else {
                                	makeTree(retNode, chName);
                        	}
				*/

			}
			else {
				if (head.checkChildStackFront() == currPos ) {
					//System.out.println("WAS REMOVING FROM STACK");
					//head.popChildStack();
				}
                        	tempNode = doc.createElement(chName);
				// look up and get proper attribute names and it's value
                        	if ( !(attrStr = head.isAttribute(chName)).equals("")  ) {
					//retVal = head.getAttribute(chName+"\\"+attrStr);
					//if ( retVal == null ) {
						retVal = head.getAttribute(head.getParentHeader(currPos)+"\\"+chName+"\\"+attrStr);
					//}
					if ( retVal instanceof Integer ) {
                                		tempNode.setAttribute(attrStr,convertData(chName, (String)dataArr.get(((Integer)retVal).intValue()), (Element)parent, attrStr));
					}
					else if ( retVal instanceof String) {
						tempNode.setAttribute(attrStr,(String)retVal);
					}
					else if ( retVal instanceof ArrayList) {
						//System.out.println("In array part for "+chName+" with no ch "+((ArrayList)retVal).size());
						Iterator itTemp = it;
						while(itTemp.hasNext()) {
							if (head.getChildHeader(((Integer)itTemp.next()).intValue()).equals(chName)) {
								itTemp.remove();
							}
						}
						it = children.iterator();
						while(it.hasNext() && ((Integer)it.next()).intValue() != currPos) {}
						for (int j = 0; j < ((ArrayList)retVal).size(); j++) {
							if (j != 0) {
								tempNode = doc.createElement(chName);
							}
							tempNode.setAttribute(attrStr,(String)((ArrayList)retVal).get(j));
							if ((retInt = head.isData(head.getParentHeader(currPos)+"\\"+chName+"\\"+attrStr+"\\"+(String)((ArrayList)retVal).get(j))) != -1 ) {
								if ( (retInt+1) > dataArr.size()) {
									System.out.println("FAILED: dataArr.get(retInt), array bounds exceeded "+(retInt+1)+" > "+dataArr.size());
								}
								tempText = doc.createTextNode(convertData(chName, (String)dataArr.get(retInt), (Element)parent, ""));
								tempNode.appendChild(tempText);
							}
							//System.out.println("About to use "+chName);
							// check to see if this node exists already so as to not duplicate it, and
							// recurse using this child as the new parent
							if ((retNode = compare((Element)parent, tempNode)) == null) {
								parent.appendChild(tempNode);
								makeTree(tempNode, chName);
							} else {
								if ((hasTextData(retNode)) != null) {
									(hasTextData(retNode)).setNodeValue(tempText.getNodeValue());
								}
								makeTree(retNode, chName);
                        				}
						}
						continue;
					}
                        	}
				// checks to see if this node should have data and set it properly if so
                        	if ( (retInt = head.isData(head.getParentHeader(currPos)+"\\"+chName)) != -1 ) {
					if ( (retInt+1) > dataArr.size()) {
						System.out.println("FAILED: dataArr.get(retInt), array bounds exceeded "+(retInt+1)+" > "+dataArr.size());
					}
                               		tempText = doc.createTextNode(convertData(chName, (String)dataArr.get(retInt), (Element)parent, ""));
                                	tempNode.appendChild(tempText);
                        	}
				//System.out.println("About to use "+chName);
				// check to see if this node exists already so as to not duplicate it, and
				// recurse using this child as the new parent
				if ((retNode = compare((Element)parent, tempNode)) == null) {
					parent.appendChild(tempNode);
					makeTree(tempNode, chName);
				} else {
					if ((hasTextData(retNode)) != null) {
						(hasTextData(retNode)).setNodeValue(tempText.getNodeValue());
					}
					makeTree(retNode, chName);
				}
			}
                }
        }

	/*! \brief Helper for the makeTree function
	*
	*   Takes care of the unusual case when the child specifies exactly which parent
	*   it should nest under, ex: {name=Ref Oil}Input/AEEI.
	*
	*/
	private void makeTreeHelper( Node parent, String name ) {
		ArrayList children = head.getIndecesWhereParentHeaderIs(name);
		String parentName;
		String attrStr;
		Element tempNode;
		Node retNode;
		int retInt = 0;
		boolean isExt = false;
		// go through all the children, and create their parent as they specify
		//System.out.println("Got here from"+name+" with ch# "+children.size());
		for (int i = 0; i < children.size(); i++ ) {
			// need to parse this parent header as they will not come in parsed
			parentName = head.getParentHeader(((Integer)children.get(i)).intValue());
			if (head.checkChildStackFront() == ((Integer)children.get(i)).intValue()) {
				//System.out.println("REMOVING FROM STACK");
				//head.popChildStack();
				parentName = parentName.substring(parentName.lastIndexOf('/')+1);
			}
			//if (parentName.split("/").length > 1) {
			if (parentName.matches(".*\\..*")) {
				//System.out.println("ADDING FROM STACK");
				head.addToChildStack(((Integer)children.get(i)).intValue());
				parentName = parentName.substring(0,parentName.indexOf("/"));
				isExt = true;
			}
			if (parentName.startsWith("{")) {
				retInt = parentName.indexOf("}");
				attrStr = parentName.substring(1,retInt);
				tempNode = doc.createElement(parentName.substring(retInt+1, parentName.length()));
				if (isExt) {
					parentName = '@' + parentName.substring(retInt+1);
				}
				retInt = attrStr.indexOf("=");
				tempNode.setAttribute(attrStr.substring(0,retInt), attrStr.substring(retInt+1, attrStr.length()));
			}
			else {
				//System.out.println("How did i get here");
				tempNode = doc.createElement(parentName);
				if (isExt) {
					parentName = '@' + parentName;
				}
			}
			//System.out.println("About to use "+parentName);
			// same idea as makeTree, where we need to avoid duplicates, the child should now be
			// able to recurse normally
			if ((retNode = compare((Element)parent, tempNode)) == null) {
                               	parent.appendChild(tempNode);
                                makeTree(tempNode, parentName);
                        } else {
                                makeTree(retNode, parentName);
                        }
			if(isExt) {
				//System.out.println("Now REMOVING FROM STACK");
				head.popChildStack();
			}
		}
	}
	private void finalize(Node curr) {
		if(curr.getNodeName().indexOf(":") != -1 && curr.getNodeName().indexOf(":") != 0) {
			Element temp = doc.createElement(curr.getNodeName().substring(0,curr.getNodeName().indexOf(":")));
			NamedNodeMap nm = curr.getAttributes();
			for(int i = 0; i < nm.getLength(); ++i) {
				temp.setAttributeNode(((Element)curr).removeAttributeNode((Attr)nm.item(i)));
				--i;
			}
			curr.getParentNode().replaceChild(temp, curr);
			Node tempChild;
			while((tempChild = curr.getFirstChild()) != null) {
				temp.appendChild(curr.removeChild(tempChild));
				finalize(tempChild);
			}

			//curr.setNodeName(curr.getNodeName().substring(0,curr.getNodeName().indexOf(":")));
		} else {
			NodeList nl = curr.getChildNodes();
			for( int i = 0; i < nl.getLength(); ++i) {
				finalize(nl.item(i));
			}
		}
	}
}
