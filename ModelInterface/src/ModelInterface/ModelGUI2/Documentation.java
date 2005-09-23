package ModelInterface.ModelGUI2;

import java.util.Vector;
import java.util.TreeSet;
import java.util.Comparator;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Set;
import java.util.Iterator;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JOptionPane;
import javax.swing.JDialog;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.Box;
import javax.swing.JTabbedPane;
import javax.swing.text.JTextComponent;
import javax.swing.BoxLayout;
import javax.swing.BorderFactory;
import javax.swing.JTextField;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.JSeparator;
import javax.swing.ListSelectionModel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.PrintStream;
import java.net.URI;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

import javax.xml.xpath.XPathFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathConstants;

import org.w3c.dom.ls.LSParser;
import org.w3c.dom.ls.LSInput;

public class Documentation {
	private Vector<DocumentationElement> documentations;
	private Document doc;
	private Comparator<Node> nodeComparator;
	private XPath xpathImpl;
	private class DocumentationElement {
		Vector<String> xpathLinks;
		Vector<TreeSet<Node>> nodeSets;
		String source;
		String sourceDate;
		String info;
		String documentationAuthor;
		public DocumentationElement(Node parseNode) {
			NodeList childList = parseNode.getChildNodes();
			nodeSets = new Vector<TreeSet<Node>>();
			xpathLinks = new Vector<String>();
			for(int i = 0; i < childList.getLength(); ++i) {
				if(childList.item(i).getNodeName().equals("XPathLink")) {
					xpathLinks.add(childList.item(i).getFirstChild().getNodeValue());
					nodeSets.add(evaluateXPath(childList.item(i).getFirstChild().getNodeValue()));
				} else if(childList.item(i).getNodeName().equals("source")) {
					source = childList.item(i).getFirstChild().getNodeValue();
				} else if(childList.item(i).getNodeName().equals("sourceDate")) {
					sourceDate = childList.item(i).getFirstChild().getNodeValue();
				} else if(childList.item(i).getNodeName().equals("info")) {
					info = childList.item(i).getFirstChild().getNodeValue();
				} else if(childList.item(i).getNodeName().equals("documentationAuthor")) {
					documentationAuthor = childList.item(i).getFirstChild().getNodeValue();
				}
			}
		}

		public DocumentationElement(String sourceIn, String sourceDateIn, String infoIn, String documentationAuthorIn) {
			nodeSets = new Vector<TreeSet<Node>>();
			xpathLinks = new Vector<String>();
			source = sourceIn;
			sourceDate = sourceDateIn;
			info = infoIn;
			documentationAuthor = documentationAuthorIn;
		}

		public boolean contains(Node n) {
			for(int i = 0; i < nodeSets.size(); ++i) {
				if(nodeSets.get(i).contains(n)) {
					return true;
				}
			}
			return false;
		}

		public void addXPathLinkForNode(Node n) {
			String xPath = nodeToXPath(n).toString();
			String ret;
			for(int i = 0; i < xpathLinks.size(); ++i) {
				System.out.println("Results of mergeing "+xpathLinks.get(i)+" with "+xPath+" is:");
				ret = meregeXPathLinks(xpathLinks.get(i), xPath);
				System.out.println(ret);
				if(ret != null) {
					xpathLinks.set(i, ret);
					nodeSets.get(i).add(n);
					return;
				}
			}
			xpathLinks.add(xPath);
			TreeSet<Node> tempSet = new TreeSet<Node>(nodeComparator);
			tempSet.add(n);
			nodeSets.add(tempSet);
			//nodeSets.add(evaluateXPath(xPath);
			// maybe try to merege with existing xpaths
		}

		private String meregeXPathLinks(String existingPath, String newPath) {
			String[] path1Arr = existingPath.split("/");
			String[] path2Arr = newPath.split("/");
			StringBuffer strBuff = new StringBuffer();
			boolean madeChange = false;
			if(path1Arr.length != path2Arr.length) {
				System.out.println("Can't merge "+existingPath+" and "+newPath);
				return null;
			}
			for(int i = 1; i < path1Arr.length; ++i) {
				if(path1Arr[i].equals(path2Arr[i])) {
					strBuff.append("/").append(path1Arr[i]);
				} else if(madeChange) {
					return null;
				} else if(path1Arr[i].indexOf('[') == -1 && path2Arr[i].indexOf('[') == -1) {
					return null;
				} else if(path1Arr[i].indexOf('[') != -1 && path2Arr[i].indexOf('[') != -1 && !path1Arr[i].substring(0, 
							path1Arr[i].indexOf('[')).equals(path2Arr[i].substring(0, path2Arr[i].indexOf('[')))) {
					return null;
				} else if(path1Arr[i].indexOf('[') == -1 && path2Arr[i].indexOf('[') != -1) {
					strBuff.append("/").append(path2Arr[i]);
				} else {
					String[] attrs = path1Arr[i].substring(path1Arr[i].indexOf('[')+1, path1Arr[i].indexOf(']')).split(" or ");
					String p2Attr = path2Arr[i].substring(path2Arr[i].indexOf('[')+1, path2Arr[i].indexOf(']'));
					boolean found = false;
					for(int j = 0; j < attrs.length && !found; ++j) {
						if(attrs[j].equals(p2Attr)) {
							strBuff.append("/").append(path1Arr[i]);
							found = true;
						}
					}
					if(!found) {
						strBuff.append("/").append(path1Arr[i].substring(0,path1Arr[i].length()-1)).append(" or ").append(p2Attr).append("]");
					}
					madeChange = true;
				}
			}
			return strBuff.toString();
		}

		private StringBuffer nodeToXPath(Node n) {
			if(n.getNodeType() != Node.DOCUMENT_NODE) {
				StringBuffer buf = nodeToXPath(n.getParentNode());
				if(n.getNodeType() == Node.TEXT_NODE) {
					return buf.append("node()");
				}
				buf.append(n.getNodeName());
				NamedNodeMap nm = n.getAttributes();
				if(nm.getLength() > 0) {
					buf.append("[");
				}
				if(nm.getLength() > 1) {
					buf.append("(");
				}
				for(int i = 0; i < nm.getLength(); ++i) {
					buf.append("(@").append(nm.item(i).getNodeName()).append("='")
						.append(nm.item(i).getNodeValue()).append("')");
					if(i+1 != nm.getLength()) {
						buf.append(" and ");
					}
				}
				if(nm.getLength() > 1) {
					buf.append(")");
				}
				if(nm.getLength() > 0) {
					buf.append("]");
				}
				return buf.append("/");
			} else {
				return new StringBuffer("/");
			}
		}

		private TreeSet<Node> evaluateXPath(String xpLink) {
			try {
				TreeSet<Node> ret = new TreeSet<Node>(nodeComparator);
				XPathExpression xpe = xpathImpl.compile(xpLink);
				NodeList nl = (NodeList)xpe.evaluate(doc.getDocumentElement(), XPathConstants.NODESET);
				for(int i = 0; i < nl.getLength(); ++i) {
					ret.add(nl.item(i));
				}
				return ret;
			} catch(XPathExpressionException xpee) {
				xpee.printStackTrace();
			}
			return null;
		}

		void toXML(PrintStream docOutStream) {
			docOutStream.println("\t<documentationElement>");
			for(int i = 0; i < xpathLinks.size(); ++i) {
				docOutStream.println("\t\t<XPathLink>"+xpathLinks.get(i)+"</XPathLink>");
			}
			docOutStream.println("\t\t<source>"+source+"</source>");
			docOutStream.println("\t\t<sourceDate>"+sourceDate+"</sourceDate>");
			docOutStream.println("\t\t<info>"+info+"</info>");
			docOutStream.println("\t\t<documentationAuthor>"+documentationAuthor+"</documentationAuthor>");
			docOutStream.println("\t</documentationElement>");
		}
	}

	public Documentation(Document docIn, final URI aDocumentationURI, LSParser lsParser, LSInput lsInput) {
		Document documentationDoc = null;
		doc = docIn;
		documentations = new Vector<DocumentationElement>();
		xpathImpl = XPathFactory.newInstance().newXPath();
		nodeComparator = new Comparator<Node> () {
			public int compare(Node obj1, Node obj2) {
				if(obj1.equals(obj2)) {
					return 0;
				} else {
					String node1Val = obj1.getNodeValue();
					String node2Val = obj2.getNodeValue();
					int ret = String.CASE_INSENSITIVE_ORDER.compare(node1Val, node2Val);
					if(ret == 0) {
						return 1;
					} else {
						return ret;
					}
				}
			}
		};

		try {
			lsInput.setByteStream(new FileInputStream(new File(aDocumentationURI)));
			documentationDoc = lsParser.parse(lsInput);
		} catch (Exception e) {
			System.out.println("Got Exception while creating XML document: "
					+ e);
			return;
		}
		NodeList childList = documentationDoc.getDocumentElement().getChildNodes();
		for(int i = 0; i < childList.getLength(); ++i) {
			documentations.add(new DocumentationElement(childList.item(i)));
		}

		ModelInterface.InterfaceMain.getInstance().addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control") && evt.getOldValue().equals(
						InputViewer.controlStr)) {
					// maybe check old value here to make sure InputViewer is loosing control
					System.out.println("The open xml file must be closing");
					System.out.println("Going to write out documentation and stop listening");
					try {
						PrintStream docOutStream = new PrintStream(new File(aDocumentationURI));
						toXML(docOutStream);
						docOutStream.close();
					} catch (FileNotFoundException fnfe) {
						fnfe.printStackTrace();
					}
					ModelInterface.InterfaceMain.getInstance().removePropertyChangeListener(this);
				}
			}
		});

	}

	private void toXML(PrintStream docOutStream) {
		docOutStream.println("<documentation>");
		for(int i = 0; i < documentations.size(); ++i) {
			documentations.get(i).toXML(docOutStream);
		}
		docOutStream.println("</documentation>");
	}

	public boolean hasDocumentation(Node n) {
		int where;
		if(n.getNextSibling() != null && n.getNextSibling().getNodeType() == Node.COMMENT_NODE) {
			where = Integer.parseInt(n.getNextSibling().getNodeValue());
		} else {
			where = contains(n);
			n.getParentNode().appendChild(doc.createComment(String.valueOf(where)));
		}
		return where != -1;
	}

	public void getDocumentation(Vector<Node> selectedNodes) {
		getDocumentation(selectedNodes, null, null);
	}

	public void getDocumentation(Vector<Node> selectedNodes, int[] rows, int[] cols) {
		int where;
		int row = 0;
		int col = 0;
		final Map<Integer, LinkedList<String>> docMaps = new HashMap<Integer, LinkedList<String>>();
		Vector<String> notFoundNames = new Vector<String>();
		for(Iterator<Node> i = selectedNodes.iterator(); i.hasNext(); ) {
			if(cols != null && col == cols.length) {
				col = 0;
				++row;
			}
			//Node n = selectedNodes.get(i);
			Node n = i.next();
			if(n.getNextSibling() != null && n.getNextSibling().getNodeType() == Node.COMMENT_NODE) {
				where = Integer.parseInt(n.getNextSibling().getNodeValue());
				/*
				if(where != -1) {
					System.out.println("Cached Has Documentation at: "+where);
				} else {
					System.out.println("Cached Doesn't have documentation");
				}
				*/
			} else {
				where = contains(n);
				/*
				if((where = contains(n)) != -1) {
					System.out.println("Has documentation at: "+where);
				} else {
					System.out.println("Doesn't have documentation");
				}
				*/
				n.getParentNode().appendChild(doc.createComment(String.valueOf(where)));
			}
			if(where != -1) {
				if(docMaps.containsKey(where)) {
					if(rows == null || cols == null) {
						//docMaps.get(where).addFirst(n);
						docMaps.get(where).addFirst(n.getNodeValue());
					} else {
						docMaps.get(where).addFirst("("+rows[row]+", "+cols[col]+")");
					}
				} else {
					LinkedList<String> tempSet = new LinkedList<String>();
					if(rows == null || cols == null) {
						//tempSet.addFirst(n);
						tempSet.addFirst(n.getNodeValue());
					} else {
						tempSet.addFirst("("+rows[row]+", "+cols[col]+")");
					}
					docMaps.put(where, tempSet);
				}
				i.remove();
			} else {
				if(rows == null || cols == null) {
					notFoundNames.add(n.getNodeValue());
				} else {
					notFoundNames.add("("+rows[row]+", "+cols[col]+")");
				}
			}
			++col;
		}
		if(selectedNodes.size() != 0) {
			// pop add dialog
			addToDocumentation(selectedNodes, notFoundNames, docMaps);
		}
		if(docMaps.size() == 0) {
			/*
			JOptionPane.showMessageDialog(ModelInterface.InterfaceMain.getInstance(), "Couldn't find any documentation", 
					"Annotation Error", JOptionPane.WARNING_MESSAGE);
			*/
			return;
		}
		final JDialog docDialog = new JDialog(ModelInterface.InterfaceMain.getInstance(), "Annotation", false);
		docDialog.setLocation(100,100);
		docDialog.setResizable(false);
		JButton cancelButton = new JButton("Cancel");
		JButton okButton = new JButton("  OK  ");
		JPanel all = new JPanel();
		JPanel tempPanel;
		Component seperator = Box.createRigidArea(new Dimension(20, 10));
		JTabbedPane tp = new JTabbedPane();
		Set<Integer> keySet = docMaps.keySet();
		final Vector<Vector<JTextComponent>> textFields = new Vector<Vector<JTextComponent>>(keySet.size(), 0);
		for(Iterator<Integer> it = keySet.iterator(); it.hasNext(); ) {
			all = new JPanel();
			all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
			all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
			int currDoc = it.next().intValue();
			Vector<JTextComponent> fieldsTemp = new Vector<JTextComponent>(4, 0);

			JTextComponent tempTextComp = new JTextField(documentations.get(currDoc).source, 50);
			fieldsTemp.add(tempTextComp);
			tempPanel = new JPanel();
			tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
			tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
			tempPanel.add(new JLabel("Source: "));
			tempPanel.add(seperator);
			tempPanel.add(tempTextComp);
			tempPanel.add(Box.createHorizontalGlue());
			all.add(tempPanel);

			tempTextComp = new JTextField(documentations.get(currDoc).sourceDate, 50);
			fieldsTemp.add(tempTextComp);
			tempPanel = new JPanel();
			tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
			tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
			tempPanel.add(new JLabel("Source Date: "));
			tempPanel.add(seperator);
			tempPanel.add(tempTextComp);
			tempPanel.add(Box.createHorizontalGlue());
			all.add(tempPanel);

			tempTextComp = new JTextArea(documentations.get(currDoc).info, 5, 50);
			fieldsTemp.add(tempTextComp);
			tempPanel = new JPanel();
			tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
			tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
			tempPanel.add(new JLabel("Info: "));
			tempPanel.add(seperator);
			tempPanel.add(new JScrollPane(tempTextComp));
			tempPanel.add(Box.createHorizontalGlue());
			all.add(tempPanel);

			tempTextComp = new JTextField(documentations.get(currDoc).documentationAuthor, 50);
			fieldsTemp.add(tempTextComp);
			tempPanel = new JPanel();
			tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
			tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
			tempPanel.add(new JLabel("Documentation Author: "));
			tempPanel.add(seperator);
			tempPanel.add(tempTextComp);
			tempPanel.add(Box.createHorizontalGlue());
			all.add(tempPanel);

			LinkedList<String> tempSet = docMaps.get(currDoc);
			StringBuffer strBuff;
			tempPanel = new JPanel();
			tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
			tempPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 5, 10));
			if(rows == null || cols == null) {
				tempPanel.add(new JLabel("Applies to Selected Nodes with Values:"));
			} else {
				tempPanel.add(new JLabel("Applies to Selected Nodes with Positions:"));
			}
			tempPanel.add(Box.createHorizontalGlue());
			all.add(tempPanel);
			int i = 0;
			while(i < tempSet.size()) {
				strBuff = new StringBuffer();
				for(int j = 0; j < 10 && i < tempSet.size(); ++j) {
					strBuff.append(tempSet.get(i)).append(", ");
					++i;
				}
				strBuff.delete(strBuff.length()-2, strBuff.length()-1);
				tempPanel = new JPanel();
				tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
				tempPanel.setBorder(BorderFactory.createEmptyBorder(0, 20, 10, 10));
				tempPanel.add(new JLabel(strBuff.toString()));
				tempPanel.add(Box.createHorizontalGlue());
				all.add(tempPanel);
			}

			/*
			all.add(new JLabel("This documentation applies to fitting XPaths: "));
			for(int i = 0; i < documentations.get(currDoc).xpathLinks.size(); ++i) {
				all.add(new JLabel("\t"+documentations.get(currDoc).xpathLinks.get(i)));
			}
			*/

			textFields.add(fieldsTemp);
			tp.addTab(documentations.get(currDoc).source, all);
		}

		all = new JPanel();
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		all.add(tp);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(okButton);
		tempPanel.add(seperator);
		tempPanel.add(cancelButton);
		all.add(tempPanel);

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Set<Integer> keySet = docMaps.keySet();
				int pos = 0;
				for(Iterator<Integer> it = keySet.iterator(); it.hasNext(); ) {
					int currDoc = it.next().intValue();
					documentations.get(currDoc).source = textFields.get(pos).get(0).getText();
					documentations.get(currDoc).sourceDate = textFields.get(pos).get(1).getText();
					documentations.get(currDoc).info = textFields.get(pos).get(2).getText();
					documentations.get(currDoc).documentationAuthor = textFields.get(pos).get(3).getText();
					++pos;
				}
				docDialog.dispose();
			}
		});
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				docDialog.dispose();
			}
		});

		docDialog.getContentPane().add(all);
		docDialog.pack();
		docDialog.setVisible(true);
	}

	private int contains(Node n) {
		for(int i = 0; i < documentations.size(); ++i) {
			if(documentations.get(i).contains(n)) {
				return i;
			}
		}
		return -1;
	}

	private void addToDocumentation(final Vector<Node> selectedNodes, final Vector<String> notFoundNames, 
			final Map<Integer, LinkedList<String>> docMaps) {
		if(JOptionPane.showConfirmDialog(ModelInterface.InterfaceMain.getInstance(), 
			"Warning Some of the nodes selected did not have documentation\n Would you like to add documentation now?",
			"Missing Documentation", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.NO_OPTION) {
			return;
		}
		final Vector<String> docNames = new Vector<String>(documentations.size());
		for(int i = 0; i < documentations.size(); ++i) {
			docNames.add(documentations.get(i).source + "  Date: "+documentations.get(i).sourceDate);
		}
		final JDialog addDocDialog = new JDialog(ModelInterface.InterfaceMain.getInstance(), "Add Documentation", true);
		addDocDialog.setLocation(100,100);
		addDocDialog.setResizable(false);
		JButton doneButton = new JButton("Done");
		JButton updateButton = new JButton("Update");
		JButton newButton = new JButton("New Documentation");
		JPanel all = new JPanel();
		final JList nodesList = new JList(notFoundNames);
		nodesList.getSelectionModel().setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		nodesList.setPreferredSize(new Dimension(100, 60));
		final JList docsList = new JList(docNames);
		docsList.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		docsList.setPreferredSize(new Dimension(300, 60));
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		JPanel tempPanel;

		updateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Object[] nodeNamesSel = nodesList.getSelectedValues();
				int[] nodesSel = nodesList.getSelectedIndices();
				Vector<Node> selNodes = new Vector<Node>(nodesSel.length, 0);
				LinkedList<String> tempSet;
				if(nodesSel.length == 0) {
					// error
					System.out.println("No nodes selected");
					return;
				}
				int docSel = docsList.getSelectedIndex();
				if(docSel == -1) {
					// error
					System.out.println("no doc selected");
					return;
				}
				if(!docMaps.containsKey(new Integer(docSel))) {
					tempSet = new LinkedList<String>();
					docMaps.put(docSel, tempSet);
				} else {
					tempSet = docMaps.get(docSel);
				}
				for(int i = 0; i < nodesSel.length; ++i) {
					documentations.get(docSel).addXPathLinkForNode(selectedNodes.get(i));
					Node n = selectedNodes.get(i);
					if(n.getNextSibling() != null && n.getNextSibling().getNodeType() == Node.COMMENT_NODE) {
						n.getNextSibling().setNodeValue(String.valueOf(docSel));
					} else {
						n.getParentNode().appendChild(doc.createComment(String.valueOf(docSel)));
					}
					selNodes.add(selectedNodes.get(i));
					notFoundNames.remove((String)nodeNamesSel[i]);
					tempSet.addFirst((String)nodeNamesSel[i]);
				}
				for(int i = 0; i < nodesSel.length; ++i) {
					selectedNodes.remove(selNodes.get(i));
				}
				//((AbstractListModel)nodesList.getModel()).fireIntervalRemoved(this, 0, nodesSel[nodesSel.length-1]);
				nodesList.updateUI();
			}
		});

		newButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(newDocumentationDialog()) {
					docNames.add(documentations.get(documentations.size()-1).source + 
						"  Date: "+documentations.get(documentations.size()-1).sourceDate);
					docsList.updateUI();
				}
				// create new documentation element
				// add it to documentations
				// add it to docNames
				// ??maybe let the list know to update
			}
		});

		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addDocDialog.setVisible(false);
			}
		});

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(new JLabel("Nodes:"));
		tempPanel.add(Box.createHorizontalStrut(95));
		tempPanel.add(new JLabel("Documentations:"));
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JScrollPane(nodesList));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(new JSeparator(javax.swing.SwingConstants.VERTICAL));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(new JScrollPane(docsList));
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(updateButton);
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(newButton);
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(doneButton);
		all.add(tempPanel);

		addDocDialog.getContentPane().add(all);
		addDocDialog.pack();
		addDocDialog.setVisible(true);
	}

	public boolean newDocumentationDialog() {
		final JDialog newDocDialog = new JDialog(ModelInterface.InterfaceMain.getInstance(), "Add Documentation", true);
		newDocDialog.setLocation(100,100);
		newDocDialog.setResizable(false);
		int prevDocSize = documentations.size();
		JButton okButton = new JButton("  OK  ");
		JButton cancelButton = new JButton("Cancel");

		JPanel all = new JPanel();
		JPanel tempPanel;
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));
		all.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

		final JTextField sourceField = new JTextField(50);
		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Source: "));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(sourceField);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		final JTextField sourceDateField = new JTextField(50);
		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Source Date: "));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(sourceDateField);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		final JTextArea infoField = new JTextArea(5, 50);
		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Info: "));
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(new JScrollPane(infoField));
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		final JTextField docAuthorField = new JTextField(50);
		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Documentation Author: "));
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(docAuthorField);
		tempPanel.add(Box.createHorizontalGlue());
		all.add(tempPanel);

		tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(Box.createHorizontalGlue());
		tempPanel.add(okButton);
		tempPanel.add(Box.createHorizontalStrut(10));
		tempPanel.add(cancelButton);
		all.add(tempPanel);

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				documentations.add(new DocumentationElement(sourceField.getText(), sourceDateField.getText(),
						infoField.getText(), docAuthorField.getText()));
				newDocDialog.setVisible(false);
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newDocDialog.setVisible(false);
			}
		});

		newDocDialog.getContentPane().add(all);
		newDocDialog.pack();
		newDocDialog.setVisible(true);
		return prevDocSize != documentations.size();
	}
}
