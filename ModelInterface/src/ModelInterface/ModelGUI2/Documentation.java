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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.net.URI;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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
	private Comparator nodeComparator;
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

		public boolean contains(Node n) {
			for(int i = 0; i < nodeSets.size(); ++i) {
				if(nodeSets.get(i).contains(n)) {
					return true;
				}
			}
			return false;
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
	}

	public Documentation(Document docIn, final URI aDocumentationURI, LSParser lsParser, LSInput lsInput) {
		Document documentationDoc = null;
		doc = docIn;
		documentations = new Vector<DocumentationElement>();
		xpathImpl = XPathFactory.newInstance().newXPath();
		nodeComparator = new Comparator() {
			public int compare(Object obj1, Object obj2) {
				if(obj1.equals(obj2)) {
					return 0;
				} else {
					String node1Val = ((Node)obj1).getNodeValue();
					String node2Val = ((Node)obj2).getNodeValue();
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
	}

	public void getDocumentation(Vector<Node> selectedNodes) {
		getDocumentation(selectedNodes, null, null);
	}

	public void getDocumentation(Vector<Node> selectedNodes, int[] rows, int[] cols) {
		int where;
		int row = 0;
		int col = 0;
		final Map<Integer, LinkedList<String>> docMaps = new HashMap<Integer, LinkedList<String>>();
		for(int i = 0; i < selectedNodes.size(); ++i) {
			if(cols != null && col == cols.length) {
				col = 0;
				++row;
			}
			Node n = selectedNodes.get(i);
			if(n.getNextSibling() != null && n.getNextSibling().getNodeType() == Node.COMMENT_NODE) {
				where = Integer.parseInt(n.getNextSibling().getNodeValue());
				if(where != -1) {
					System.out.println("Cached Has Documentation at: "+where);
				} else {
					System.out.println("Cached Doesn't have documentation");
				}
			} else {
				if((where = contains(n)) != -1) {
					System.out.println("Has documentation at: "+where);
				} else {
					System.out.println("Doesn't have documentation");
				}
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
			}
			++col;
		}
		if(docMaps.size() == 0) {
			// error
			JOptionPane.showMessageDialog(ModelInterface.InterfaceMain.getInstance(), "Couldn't find any documentation", 
					"Annotation Error", JOptionPane.WARNING_MESSAGE);
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
				for(Iterator<Integer> it = keySet.iterator(); it.hasNext(); ) {
					int currDoc = it.next().intValue();
					documentations.get(currDoc).source = textFields.get(currDoc).get(0).getText();
					documentations.get(currDoc).sourceDate = textFields.get(currDoc).get(1).getText();
					documentations.get(currDoc).info = textFields.get(currDoc).get(2).getText();
					documentations.get(currDoc).documentationAuthor = textFields.get(currDoc).get(3).getText();
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
}
