import java.util.*;
import javax.swing.table.AbstractTableModel;
import org.w3c.dom.*;
//import javax.swing.table.*;
//import javax.swing.JTable;
//import java.awt.Component;
import javax.swing.JScrollPane;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.TreePath;
import java.awt.event.*;
import org.apache.xpath.domapi.*;
import org.w3c.dom.xpath.*;

public abstract class BaseTableModel extends AbstractTableModel {
	protected Vector activeRows;
	protected Document doc;
	protected ArrayList wild;
	protected Map tableFilterMaps;
	protected Frame parentFrame;

	// stuff for filtering
	// can i move these somewhere
	protected int currFilter;
	protected String[] currKeys;
	
	protected String tableTypeString;

	public BaseTableModel() {}
	public BaseTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString) {
		this.doc = doc;
		this.parentFrame = parentFrame;
		this.tableTypeString = tableTypeString;
	}
	
	public abstract void flip(int row, int col);
	protected abstract void buildTable(XPathExpression xpe);
 	protected XPathExpression treePathtoXPath(TreePath tp, Node currNode, int flag) {
           XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
           String pathStr = "";
           Object[] path = tp.getPath();
           Node tempNode;
	   currNode = ((DOMmodel.DOMNodeAdapter)path[path.length-1]).getNode();
           for (int i = 0; i < path.length-1; i++) {
	           tempNode= ((DOMmodel.DOMNodeAdapter)path[i]).getNode();
		   if(flag == 0) {
                   	pathStr = pathStr + tempNode.getNodeName() + "/";
		   } else if(flag == 1) { 
			   pathStr = pathStr + tempNode.getNodeName(); 
			   Vector attrs = getAttrsNoWild(tempNode);
			   if(attrs.size() > 0) {
				   pathStr = pathStr + "[";
			   }
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "(@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"')";
				   if(j < attrs.size()-1) {
					   pathStr = pathStr + " and ";
				   } else { 
					   pathStr = pathStr + "]";
				   }
			   }
			   pathStr = pathStr + "/";
		   }
           }
           pathStr = "//" + pathStr + currNode.getNodeName();
           if (flag == 1 &&currNode.hasAttributes() && !getTextData(currNode).equals("")) {
		   if(flag == 0) {
                   	pathStr = pathStr + "[@" + getOneAttrVal(currNode) + "]";
		   } else if(flag == 1) {
			   Vector attrs = getAttrsNoWild(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "[@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"']";
			   }
			   pathStr = pathStr+ "/node()";
		   }
           }
           else if (flag == 1 && currNode.hasAttributes()) {
		   if (flag == 0) {
                   	pathStr = pathStr + "[@" + getOneAttrVal(currNode) + "]/node()";
		   } else if(flag == 1) {
			   Vector attrs =getAttrsNoWild(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"'";
			   }
			   pathStr = pathStr+ "/node()";
		   }
           }
           else {
                   pathStr = pathStr + "/node()";
           }
           return xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(currNode));
	}
  public String getOneAttrVal(Node node) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  return nodeMap.item(0).getNodeName() +"="+ nodeMap.item(0).getNodeValue();
	  //return ((Element)node).getAttribute(nodeMap.item(0).getNodeValue());
  }

  public String getOneAttrVal(Node node, int pos) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  return nodeMap.item(pos).getNodeName() +"="+ nodeMap.item(pos).getNodeValue();
	  //return ((Element)node).getAttribute(nodeMap.item(0).getNodeValue());
  }

  public Vector getAttrsNoWild(Node node) {
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && node.getNodeName().equals("subsector") ) {
		  return new Vector();
	  }

	  if(node.getNodeName().equals((String)wild.get(0)) || node.getNodeName().equals((String)wild.get(1)) ) { 
		 return new Vector();
	  }
	  NamedNodeMap nodeMap = node.getAttributes();
	  Node tempNode;
	  Vector ret = new Vector();
	  for(int i = 0; i < nodeMap.getLength(); i++) {
		  tempNode = nodeMap.item(i);
		  if(tempNode.getNodeName().indexOf(':') == -1) {
		  	ret.add(tempNode);
		  }
	  }
	  return ret;
  }

  public String getTextData(Node node) {
	  NodeList nl = node.getChildNodes();
	  for (int i = 0; i < nl.getLength(); i++) {
		  if (nl.item(i).getNodeType() == Node.TEXT_NODE) {
			  return nl.item(i).getNodeValue();
		  }
	  }
	  return "";
  }
  protected ArrayList chooseTableHeaders( TreePath path, JFrame parentFrame ){
	final ArrayList selected = new ArrayList(2);

	final Object[] itemsObjs = path.getPath();
	
	String[] items = new String[ itemsObjs.length ];
	for(int i=0; i<itemsObjs.length; i++){
		Node currNode = ((DOMmodel.DOMNodeAdapter)itemsObjs[i]).getNode();
		if( currNode.hasAttributes() ){ // print out first attribute, just to help
			NamedNodeMap nodeMap = currNode.getAttributes();
			items[i] = currNode.getNodeName() + " (" + nodeMap.item(0).getNodeName() + " = " + nodeMap.item(0).getNodeValue() + ")"; 
		}else{
			items[i] = currNode.getNodeName();
		}
	}
	final JList list = new JList(items);
	list.setSelectionMode(DefaultListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
	JScrollPane scrollingList = new JScrollPane(list);
    		
	final JDialog filterDialog = new JDialog(parentFrame, tableTypeString + " for \'" + ((DOMmodel.DOMNodeAdapter)itemsObjs[itemsObjs.length-1]).getNode().getNodeName() + "\'. Please choose two headers:", true);
	filterDialog.setSize(500,400);
	filterDialog.setLocation(100,100);
	filterDialog.setResizable(false);
	
	final JButton nextButton = new JButton(" Finished With Selection ");
	nextButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			int[] selectedIndices = list.getSelectedIndices();
			if( selectedIndices.length == 2 ){
				selected.add( itemsObjs[ selectedIndices[0] ] );
				selected.add( itemsObjs[ selectedIndices[1] ] );
				filterDialog.dispose();
			}else{
				// make user try again ..
				JOptionPane.showMessageDialog(null, "Error: You must choose exactly two (2)!");
			}
		}
	});

	JPanel buttonPane = new JPanel();
	buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
	buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	buttonPane.add(Box.createHorizontalGlue());
	buttonPane.add(nextButton);
	buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));

	final JLabel listLabel = new JLabel();
	listLabel.setHorizontalAlignment(JLabel.LEFT);

	JPanel listPane = new JPanel();
	listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
	listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	listPane.add(Box.createVerticalGlue());
	listPane.add(listLabel);
	listPane.add(Box.createVerticalStrut(10));
	JScrollPane listScroll = new JScrollPane(list);
	listScroll.setPreferredSize(new Dimension(150, 750));
	listPane.add(listScroll);
	listPane.add(Box.createVerticalStrut(10));
	listPane.add(new JSeparator(SwingConstants.HORIZONTAL));

	Container filterContent = filterDialog.getContentPane();
	//filterContent.add(new JSeparator(SwingConstants.HORIZONTAL));
	filterContent.add(listPane, BorderLayout.CENTER);
	filterContent.add(buttonPane, BorderLayout.PAGE_END);
	filterDialog.setContentPane(filterContent);
	filterDialog.show();
   		
  	return selected; //arraylist with the two selected nodes
  }
	protected void filterData(JFrame parentFrame) {
		// so i can make oldNumRows final and it won't crash
		if (activeRows == null) {
			activeRows = new Vector();
		}
		final int oldNumRows = activeRows.size();
		currKeys = new String[0];
		final Map tempFilterMaps = (Map)((LinkedHashMap)tableFilterMaps).clone();

		final Vector possibleKeys = new Vector(tempFilterMaps.keySet());
		currFilter = possibleKeys.size()-1;
		String title = "Filter Table";
		if (possibleKeys.isEmpty()) {
			return;
		}
		final JDialog filterDialog = new JDialog(parentFrame, title, true);
		filterDialog.setSize(500,400);
		filterDialog.setLocation(100,100);
		filterDialog.setResizable(false);

		final JList list = new JList();
		final JLabel listLabel = new JLabel();
		listLabel.setHorizontalAlignment(JLabel.LEFT);
		updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);

		final String cancelTitle = " Cancel ";

		final JButton cancelButton = new JButton(cancelTitle);
		final JButton backButton = new JButton(" < Back ");
		final JButton nextButton = new JButton(" Next > ");

		backButton.setMnemonic(KeyEvent.VK_B);
		nextButton.setMnemonic(KeyEvent.VK_N);
	        cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!cancelButton.getText().equals(cancelTitle)) {
					updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
					tableFilterMaps = tempFilterMaps;
					doFilter(possibleKeys);
					if (oldNumRows < activeRows.size()) {
						fireTableRowsInserted(oldNumRows, activeRows.size());
					} else if (oldNumRows > activeRows.size()) {
						fireTableRowsDeleted(0, activeRows.size());
					} else {
						fireTableRowsUpdated(0,activeRows.size());
					}
				}
				//exit this dialog..
				filterDialog.dispose();
				//filterDialog.hide();
			}
		});

	    backButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				//currFilter--;
				currFilter++;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!nextButton.isEnabled()) {
					nextButton.setEnabled(true);
					cancelButton.setText(cancelTitle);
				}
				if (currFilter == possibleKeys.size()-1) {
					backButton.setEnabled(false);
				}
			}
		});

	    nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				//currFilter++;
				currFilter--;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!backButton.isEnabled()) {
					backButton.setEnabled(true);
				}
				if (currFilter == 0) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				}
			}
		});

		JPanel buttonPane = new JPanel();
	    	buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
	    	buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	    	buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(backButton);
		backButton.setEnabled(false);
		if (possibleKeys.size() == 1) {
			nextButton.setEnabled(false);
			cancelButton.setText("Finished");
		}

	    	buttonPane.add(nextButton);
	    	buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
	    	buttonPane.add(cancelButton);

		JPanel listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		listPane.add(Box.createVerticalGlue());
		listPane.add(listLabel);
		listPane.add(Box.createVerticalStrut(10));
		JScrollPane listScroll = new JScrollPane(list);
		listScroll.setPreferredSize(new Dimension(150, 750));
		listPane.add(listScroll);
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(new JSeparator(SwingConstants.HORIZONTAL));

		Container filterContent = filterDialog.getContentPane();
		//filterContent.add(new JSeparator(SwingConstants.HORIZONTAL));
		filterContent.add(listPane, BorderLayout.CENTER);
		filterContent.add(buttonPane, BorderLayout.PAGE_END);
		filterDialog.setContentPane(filterContent);
		filterDialog.show();
	}
	protected void updateFilters(Map tempFilterMaps, JList list, String key) {
		int[] selectedKeys = list.getSelectedIndices();
		int j = 0;
		for (int i = 0; i < currKeys.length; i++) {
			// clean this up... maybe
			if (((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j >= selectedKeys.length || i != selectedKeys[j])) {
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(false));
			} else if (!((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j < selectedKeys.length && i == selectedKeys[j])) {
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(true));
			}
			if (j < selectedKeys.length && i == selectedKeys[j]) {
				j++;
			}
		}
	}
	protected void updateList(JList list, JLabel listLabel, String key, Map tempFilterMaps) {
		HashMap tempMap = (HashMap)tempFilterMaps.get(key);
		Vector tempVector = new Vector();
		listLabel.setText("Filter "+key);
		currKeys = (String[])tempMap.keySet().toArray(new String[0]);
		list.setListData(currKeys);
		for (int i = 0; i < currKeys.length; i++) {
			if (((Boolean)tempMap.get(currKeys[i])).booleanValue()) {
				tempVector.addElement(new Integer(i));
			}
		}
		int[] selected = new int[tempVector.size()];
		for (int i = 0; i < selected.length; i++) {
			selected[i] = ((Integer)tempVector.get(i)).intValue();
		}
		tempMap = null;
		tempVector = null;
		list.setSelectedIndices(selected);
	}
	protected abstract void doFilter(Vector possibleFilters);
}
