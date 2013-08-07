package ModelInterface.ModelGUI2.csvconv;

import java.util.*;
import org.w3c.dom.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import ModelInterface.common.DataPair;

/**
*       Converts CSV formated tables to XML.
*
*	Class creates a DOM document from CSV, adds to the tree using specified header info,
*	and data array. Can also output the resulting XML to a specified file.
*
*	@author Pralit Patel
*	@author Katherine Chung
*/
public class DOMTreeBuilder {

	/**
	 * The reference to the DOM Document we are building.
	 */
	private Document doc;

	/**
	 * The set of Headers that is currently being used to process.
	 */
	private Headers head; 

	/**
	 * A Map used to keep lookup maps read from the tables.  This map
	 * is used to convert attribute values to according to a read map
	 * if such was set up in the headers/data files. It maps:
	 * <p>Tag Name -&gt; Old Attr Name+Value -&gt; New Attr Value.</p>
	 */
	private Map<String, Map<String, String>> lookUpMap;

	/**
	 * A Flag which tells if the current header set is only to set
	 * up a look-up-map.
	 */ 
	private boolean buildMap;

	/**
	 * A Map used to map a node name to what it should be renamed to.
	 * This becuase necessary when you have an arbitrary number of 
	 * nestings with the same node name.  The current header strategy
	 * is limited in that it must have different names which leads
	 * to the need to be able to rename nodes back to what they should
	 * be.
	 */
	private final Map<String, String> renameNodeMap = new HashMap<String, String>();

	/**
	 * A reading flag which indicates the current tables only purpose
	 * is to read in a renameNodeMap.  The renaming will be done before
	 * the next table is processed or if the user trys to get the tree.
	 * The headers will be null when this flag is set.
	 */
	private boolean renameNodeNames;

	/** Default constructor.
	* Initializes the lookUpMap.
	*/
	public DOMTreeBuilder() {
		doc = null;
		lookUpMap = new HashMap<String, Map<String, String>>();
		buildMap = false;
		renameNodeNames = false;
	}

	/** Set the header.
	*
	*  Setting the header specified the format of the data.
	*
	*/
	public void setHeader(String headerIn) throws Exception {
		if(renameNodeNames) {
			doRenameNodes(doc.getDocumentElement());
			renameNodeNames = false;
		}
		// if the header starts with MAP that means it is
		// only intended to set up a look-up-map
		if(headerIn.matches(".*MAP,.*")) {
			buildMap = true;
			// get rid of that MAP
			headerIn = headerIn.substring(headerIn.indexOf(",")+1);
		} else {
			buildMap = false;
		}
		if(headerIn.matches(".*NODE_RENAME.*")) {
			renameNodeNames = true;
			head = null;
			return;
		}
		head = new Headers(headerIn);
		// create the document if it has been created alredy and if we have a 
		// root header we can use.
        if(!buildMap && doc == null) {
            // Create a new document.
            DOMImplementation domImpl = null;
            try {
                domImpl = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .getDOMImplementation();
            } catch (ParserConfigurationException e) {
                e.printStackTrace();
                return;
            }
            doc = domImpl.createDocument(null, head.getRoot().getChildName(),
                    null);
            /* Note we were not able to use the util directly in order
             * to avoid extra code dependencies
             doc = FileUtils.createDocument(InterfaceMain.getInstance(), null,
             head.getRoot().getChildName());
             */
        }
	}

	/** Adds the current data to the Tree.
	*
	*   calls makeTree using the data passed in and the current Headers set to
	*   specify how to create the Nodes.
	*
	*   \warning user must set a header before calling this function.
	*/
	public void addToTree (ArrayList<String> data) throws Exception {
		if(renameNodeNames) {
			addToRenameMap(data);
			return;
		}
		if (head == null && !buildMap) {
			System.out.println("Warning, no header set, skipping data");
			return;
		}
		if (buildMap) {
			addToMap(data);
		} else {
			makeTree(doc.getDocumentElement(), head.getRoot(), data);
		}
	}

	/**
	 * Get the current document.  Usually called after
	 * processing all of the data.
	 * @return The document that we have been building.
	 */
	public Document getDoc() {
		// don't do this anymore until I can 
		// figure out why it was needed
		//finalize(doc.getDocumentElement());
		if(renameNodeNames) {
			doRenameNodes(doc.getDocumentElement());
			renameNodeNames = false;
		}
		return doc;
	}

	/**
	 * Process the current line of data to fill in the look-up-map.
	 * It seems the map.
	 * @param dataArr The current row from the data table. 
	 */
	private void addToMap(List<String> dataArr) {
		Map<String, String> tempMap;
		// get the attr map for this tag name, or create a new one if
		// it does not exist already
		if(lookUpMap.containsKey(head.getHeader(0).getChildName())) {
			tempMap = lookUpMap.get(head.getHeader(0).getChildName());
		} else {
			tempMap = new HashMap<String, String>();
		}
		// If we have 3 data values then we are mapping:
		// "parent's attr Name+Value"&&"this attr Name+Value" -> new attrValue
		if (dataArr.size() == 3) {
			tempMap.put((String)dataArr.get(0) + "&&" + dataArr.get(1),dataArr.get(2));
		} else {
			tempMap.put(dataArr.get(0),dataArr.get(1));
		}
		// put for all headers as they are to share the same map
		for(int i = 0; i < head.getNumHeaders(); i++) {
			lookUpMap.put(head.getHeader(i).getChildName(),tempMap);
		}
	}

	/**
	 * Process the current line of data to fill in the node rename map.
	 * @param dataArr The current row from the data table. 
	 */
	private void addToRenameMap(List<String> dataArr) {
		renameNodeMap.put(dataArr.get(0), dataArr.get(1));
	}

	/**
	 * Renames a node name if the node name appears in the renameNodeMap to what
	 * ever it points to.  This will recursively process the whole tree so excessive
	 * calls should be avoided.
	 * @param node The current node being processed.
	 */
	public void doRenameNodes(Node node) {
		String renameTo = renameNodeMap.get(node.getNodeName());
		if(renameTo != null) {
			node = doc.renameNode(node, "", renameTo);
		}
		NodeList nl = node.getChildNodes();
		for( int i = 0; i < nl.getLength(); ++i) {
			doRenameNodes(nl.item(i));
		}
	}

	/**
	 * Converts any attributes according to the look-up-map should
	 * they be in the map.
	 * @param currElement The current elemet that is being processed.
	 * @param parent The parent of the element that is being processed. 
	 */ 
	private void convertData(Element currElement, Element parent) {
		// get the attr map for this tag name, if the tag is not
		// in the map then there is no converting to do.
		Map<String, String> ret = lookUpMap.get(currElement.getNodeName());
		if(ret == null) {
			return ;
		}

		// go through all of this element's attributes. If we find an attribute
		// name value combo that is in the map then change the value to what 
		// ever it was mapped to.
		NamedNodeMap nnm = currElement.getAttributes();
		for(int i = 0; i < nnm.getLength(); ++i) {
			Node currAttr = nnm.item(i);
			String retStr = ret.get(currAttr.getNodeName()+currAttr.getNodeValue());
			if(retStr != null) {
				currAttr.setNodeValue(retStr);
			} else {
				// if it was not found regular don't forget to check the
				// "parent attr"&&"child attr" as it could have been one of those
				// types of maps.
				retStr = ret.get(parent.getAttribute(currAttr.getNodeName())+"&&"
						+currAttr.getNodeName()+currAttr.getNodeValue());
				if (retStr != null ) {
					currAttr.setNodeValue(retStr);
				}
			}
		}
	}

	/** Compare to Individual Elements of a DOM tree.
	*
	*   Elements are considered to be the same if they both have the same tag name,
	*   same attrubutes, and same TEXT data if any.
	*  TODO: see if using Node.isEqualNode(Node) is a better idea then making my own. 
	*
	*  @param e1 The first element that will be compared against the other.
	*  @param e2 The second element that will be compared against the other. 
	*   @return Returns true if the elements are the same, false otherwise.
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
		return true;
	}

	/** Compare all of the imediate children of e1 to e2.
	*
	*   Compares each element and if any of the children are the same as e2
	*   it shall be returned.
	*
	*  @param e1 The parent node whos children will be checked for equality with e2.
	*  @param e2 The element that we are looking for as a child under e2. 
	*   @return Returns the Node which is the same as e2 and already exists in the tree, or null.
	*/
	public static Node compare( Element e1, Element e2 ){
		// get all the immediate children of e1 and compare them to e2
		NodeList list1 = e1.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() != Element.TEXT_NODE){
				if(/*list1.item(i).isEqualNode(e2)*/compareHelper((Element)list1.item(i), e2)){
					return list1.item(i);
				}
			}
		}
		return null;
	}

	/**
	 * Attempts to merge attributes from the existing node to the new node
	 * given that they have the same node name and the new node does not 
	 * already have any attribute names in common with the existing node.
	 * The new node will have been modified only if true was returned.
	 * @param existingNode The node to merge attributes from.
	 * @param newNode The node to merge attributes to.
	 * @return True if any attributes were merged, false otherwise.
	 */
	private static boolean mergeNode(Element existingNode, Element newNode) {
		NamedNodeMap existingAttrs = existingNode.getAttributes();
		// first check to make sure there aren't any conflicts
		for(int i = 0; i < existingAttrs.getLength(); ++i) {
			if(newNode.hasAttribute(existingAttrs.item(i).getNodeName())) {
				// they have an attr name in common
				return false;
			}
		}

		// they don't have any attrs in common so add all of existingAttrs
		// to newNode
		for(int i = 0; i < existingAttrs.getLength(); ++i) {
			Node currAttr = existingAttrs.item(i);
			newNode.setAttribute(currAttr.getNodeName(), currAttr.getNodeValue());
		}
		return true;
	}

	/**
	 * Check to see if a node has next data.  If it does return it.
	 * @param curr The node to check.
	 * @return The text node if it had any text data, null otherwise.
	 */ 
	private static Node hasTextData(Node curr) {
		NodeList list1 = curr.getChildNodes();
		for(int i=0; i<list1.getLength(); i++){
			if(list1.item(i).getNodeType() == Element.TEXT_NODE){
				return list1.item(i);
			}
		}
		return null;
	}

	/** Creates the DOM tree.
	*
	*   Starts from the root of the tree, and creates nodes in the appropriate positions,
	*   determined through the headers, fills nodes with the correct current data.
	*
	* @param parent The current node which will(likely) be the parent of the nodes created
	* at this level.
	* @param getChild The header that will be used to get all of the child headers that will
	 * @param dataArr The current row from the data table. 
	* create nodes that nest under parent. 
	*/
	private void makeTree(Node parent, Header getChild, List<String> dataArr) {
		// get all of the Headers who have parent as getChild's child
		List<Header> children = head.getChildHeaders(getChild);
		// group together all Headers that should share the
		// same XML node and pair them up
		List<DataPair<List<Header>, Node>> multiList = getMultiHeaderList(children);
		Node retNode;
		Node realParent;
		for(Iterator<Header> it = children.iterator(); it.hasNext(); ) {
			realParent = parent;
			Header currHeader = it.next();
			// get the column which the current header will read from
			// if any and get the data out from that pos
			int colPos = currHeader.getReadColumnPos();
			String currColVal = null;
			if(colPos != -1) {
				currColVal = dataArr.get(colPos);
			}
			for(Iterator<DataPair<List<Header>, Node>> multiIter = multiList.iterator(); multiIter.hasNext(); ) {
				DataPair<List<Header>, Node> currPair = multiIter.next();
				// go through each group of Headers and check if the current header is in there
				if(currPair.getKey().contains(currHeader)) {
					// if it is in there get the Node which it is supposed to merge with and have the 
					// child entity create itself in DOM form and merge with the merge node.
					Element tempNode = (Element)currHeader.getChildEntity().doCreate(doc, currColVal, (Element)currPair.getValue());
					// remove the current header to indicate that it has finished processing.
					// if there are no more headers left in the group then we can go ahead and add the merged node into the tree
					currPair.getKey().remove(currHeader);
					if(currPair.getKey().size() == 0) {
						// a #text child name when does that happen??
						// if the child specifies which parent it will nest under we need
						// to get the real parent it wants
						if(currHeader.doesSpecifyParent()) {
							realParent = getRealParent(parent, currHeader);
						}
						// convert any read attributes if need be
						convertData(tempNode, (Element)realParent);
						// if the current node does not already exist append it to the tree
						// and continue recursing down the tree
						if ((retNode = compare((Element)realParent, tempNode)) == null) {
							realParent.appendChild(tempNode);
							makeTree(tempNode, currHeader, dataArr);
							// if the current header's purpose was just to 
							// fill in heiarchy so that grandparent relationships
							// can be reached make sure to clean up the extra node
							// it created.
							if(currHeader.isCompleteGPPath()) {
								realParent.removeChild(tempNode);
							}
						} else {
							// The current node already exists so just use it as the
							// next parent and continue recursing down the tree.
							if ((hasTextData(retNode)) != null) {
								// this means we should always overwrite text data?
								(hasTextData(retNode)).setNodeValue(tempNode.getFirstChild().getNodeValue());
							}
							makeTree(retNode, currHeader, dataArr);
						}
					} else {
						// if there are headers left just make sure that the merge node
						// gets updated
						currPair.setValue(tempNode);
					}
				}
			}
		}
	}

	/**
	 * Get the real parent Node for a header if it had specified its 
	 * parent or grand parent.  This method will go back up the tree
	 * until it finds the parent/GP node it was looking for then come
	 * back down the tree duplicating heiarchy if necessary.
	 * @param parent The current believed parent for the child.
	 * @param child The header that specifies its parent.
	 * @return The real parent that the child had asked for. 
	 */ 
	private Node getRealParent( Node parent, Header child) {
		Node retNode = parent;
		boolean didCreate = false;
		boolean didMerge = false;
		if(child.hasGrandParent()) {
			Stack<Node> parentStack = new Stack<Node>();
			parentStack.push(retNode);
			Node currNode = retNode;
			// follow that trail back up the tree until we find the GP
			// node we are looking for.  Along the way keep track of where
			// we have been so we can come back down.
			while(!(currNode = currNode.getParentNode()).getNodeName()
					.equals(child.getGrandParentEntity().getName())) {
				parentStack.push(currNode);
			}
			Element tempParent = (Element)child.getGrandParentEntity().doCreate(doc, null, null);
			// convert attributes if necessary before we do any comparisons
			convertData(tempParent, (Element)currNode.getParentNode());
			// check if this node alread exists, if not create it and append it to the tree
			didMerge = mergeNode((Element)currNode, tempParent);
			if ((retNode = compare((Element)currNode.getParentNode(), tempParent)) == null) {
				currNode.getParentNode().appendChild(tempParent);
				retNode = tempParent;
				didCreate = true;
			}
			// come back down the tree, if we had created a new GP it 
			// means that we are going to have to replicate the heirachy
			// coming back down.
			while(!parentStack.empty()) {
				currNode = parentStack.pop();
				if(didCreate) {
					currNode = currNode.cloneNode(false);
					retNode.appendChild(currNode);
				} else {
					Node tempNode;
					// if when we are coming back down and we are missing heirachy
					// then create it.  This can happen when a GP header was created
					// for a different purpose and at this point went in a different 
					// direction.
					if((tempNode = compare((Element)retNode, (Element)currNode)) == null) {
						currNode = currNode.cloneNode(false);
						retNode.appendChild(currNode);
					} else {
						currNode = tempNode;
					}
				}
				retNode = currNode;
			}
		}
		if(child.getParentEntity().doesSpecifyAttrValue()) {
			Element tempParent = (Element)child.getParentEntity().doCreate(doc, null, null);
			// if the heirarchy was just created this duplicated node is no
			// longer necessary as the correct parent header will be found. 
			parent = retNode.getParentNode();
			// convert attributes if necessary before we do any comparisons
			convertData(tempParent, (Element)parent);
			if(didCreate || (didMerge && !retNode.hasChildNodes())) {
				parent.removeChild(retNode);
			}
			// if the parent already exists use it, if not append the new one.
			if ((retNode = compare((Element)parent, tempParent)) == null) {
				parent.appendChild(tempParent);
				retNode = tempParent;
			}
		}
		return retNode;
	}

	/**
	 * Ewww, GROSS!!!.  Takes the list of children headers and attempts to group together
	 * all headers that would want to combine togther and form a single Node.  For example:
	 * <p>parent/+child,parent/+{year}child Yeilds &lt;parent&gt;
	 * &lt;child year="read attr value"&gt;read data value&lt;/child&gt;&lt;/parent&gt;<p>
	 * TODO: find a better way to do this.
	 * @param children The child headers to group. 
	 * @return A list of of grouped headers paired with the Node that they should update.
	 */ 
	private List<DataPair<List<Header>, Node>> getMultiHeaderList(List<Header> children) {
		List<DataPair<List<Header>, Node>> multiList = new ArrayList<DataPair<List<Header>, Node>>();
		// First I sort all the headers in the following way:
		// All headers with the same parent entities(both GP and parent) and same name
		// are grouped to gether; in that group they are further subdivided by headers
		// who's child entity have the same attribute names.
		List<List<List<Header>>> sortList = new ArrayList<List<List<Header>>>();
		for(Iterator<Header> it = children.iterator(); it.hasNext(); ) {
			Header currHeader = it.next();
			boolean found = false;
			for(Iterator<List<List<Header>>> secIt = sortList.iterator(); !found && secIt.hasNext(); ) {
				List<List<Header>> currList = secIt.next();
				// I can go down two levels here because I am garunteed that any node down two
				// levels will meet this first check.
				Header currHeader2 = currList.get(0).get(0);
				// checking if the current header has the same parents and child name, if so
				// it belongs in this group
				if(((!currHeader.hasGrandParent() && !currHeader2.hasGrandParent()) ||
							(currHeader.hasGrandParent() 
							&& currHeader.getGrandParentEntity().equals(currHeader2.getGrandParentEntity()))) 
						&& currHeader.getParentEntity().equals(currHeader2.getParentEntity()) 
						&& currHeader.getChildName().equals(currHeader2.getChildName())) {

					for(Iterator<List<Header>> attrItr = currList.iterator(); !found && attrItr.hasNext(); ) {
						List<Header> currAttrList = attrItr.next();
						// check if the attribute names are the same if so an can be further catogerized
						// in this sub-group
						if(currHeader.getChildEntity().getAttrNames().equals(currAttrList.get(0).getChildEntity().getAttrNames())) {
							currAttrList.add(currHeader);
							found = true;
						}
					}
					// base case when list is empty
					if(!found) {
						List<Header> newList = new ArrayList<Header>();
						newList.add(currHeader);
						currList.add(newList);
						found = true;
					}
				}
			}
			// base case when list is empty
			if(!found) {
				List<List<Header>> currList = new ArrayList<List<Header>>();
				List<Header> newList = new ArrayList<Header>();
				newList.add(currHeader);
				currList.add(newList);
				sortList.add(currList);
			}
		}
		// Now I will go through and create group sets from all possible group/attribute sets.
		// This is essetially doing a cross product for all sub-groups sets for each group.
		for(Iterator<List<List<Header>>> secIt = sortList.iterator(); secIt.hasNext(); ) {
			List<List<Header>> currGroup = secIt.next();
			List<List<Header>> temp = new ArrayList<List<Header>>();
			// go through each group
			for(Iterator<List<Header>> groupIter = currGroup.iterator(); groupIter.hasNext(); ) {
				List<Header> currAttr = groupIter.next();
				// want to do the original size when looping not current size
				// since as we go through the Headers tuples will be duplicated
				// and added but they have already been processed so we don't 
				// want to provess them again
				int origSize = temp.size();
				// allow it to go in then temp is size zero for the base case
				for(int i = 0; i < origSize || temp.size() == 0; ++i) {
					List<Header> currTemp;
					if(temp.size() != 0) {
						currTemp = temp.get(i);
					} else {
						currTemp = null;
					}
					for(int j = 0; j < currAttr.size(); ++j) {
						Header currHeader = currAttr.get(j);
						if(currTemp != null) {
							if(j != currAttr.size()-1) {
								// we duplicate currAttr.size() -1 tuples
								// and add the current Header to the tuple
								// avoid warnings use my own clone
								//currTemp = (List<Header>)((ArrayList<Header>)currTemp).clone();
								List<Header> newTuple = cloneList(currTemp);
								newTuple.add(currHeader);
								temp.add(newTuple);
							} else {
								// don't duplicate the tuple, but since
								// this is the last just go ahead and 
								// add the header to the tuple
								currTemp.add(currHeader);
							}
						} else {
							// this is the base case, just create a new
							// groups for the current header
							List<Header> tempAdd = new ArrayList<Header>();
							tempAdd.add(currHeader);
							temp.add(tempAdd);
						}
					}
				}
			}
			// after we have processed a group we can add all the group/node pairs to the
			// list to return
			for(Iterator<List<Header>> tempIter = temp.iterator(); tempIter.hasNext(); ) {
				multiList.add(new DataPair<List<Header>, Node>(tempIter.next(), null));
			}
		}
		return multiList;
	}

	/**
	 * Use this clone instead of ArrayList.clone().  Created this
	 * method to avoid using the clone method of the object since that
	 * returns an Object and the type information is lost and requires
	 * an unchecked cast.  This method is functionally equivalent to 
	 * ArrayList.clone() as it does a shallow copy of a list only this
	 * method keeps the type information.
	 * @param l List to copy.
	 * @return A shallow copy of l.
	 * @see ArrayList.clone() 
	 */
	private static <T> List<T> cloneList(List<T> l) {
		List<T> ret = new ArrayList<T>(l.size());
		for(Iterator<T> it = l.iterator(); it.hasNext(); ) {
			ret.add(it.next());
		}
		return ret;
	}

	/**
	 * I am not really sure what is going on here.  It seems to be here to remove
	 * some extra processing info that might have been left on during processing
	 * before we return the DOM.  The thing is I don't know when it would be the case
	 * that it would have to do anything at all.
	 * @param curr The current node being processed for finalization.
	 */
	private void finalize(Node curr) {
		// what is going on here?
		if(curr.getNodeName().indexOf(":") != -1 && curr.getNodeName().indexOf(":") != 0) {
			System.out.println("I am acctually in here!! curr: "+curr.getNodeName());
			// did I want doc.renameNode ???
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
