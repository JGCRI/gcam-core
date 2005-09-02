package ModelInterface.ModelGUI2;

import java.util.Vector;
import java.util.TreeSet;
import java.util.Comparator;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;

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
		public DocumentationElement(Node parseNode) {
			NodeList childList = parseNode.getChildNodes();
			nodeSets = new Vector<TreeSet<Node>>();
			xpathLinks = new Vector<String>();
			for(int i = 0; i < childList.getLength(); ++i) {
				if(childList.item(i).getNodeName().equals("XPathLink")) {
					xpathLinks.add(childList.item(i).getFirstChild().getNodeValue());
					nodeSets.add(evaluateXPath(childList.item(i).getFirstChild().getNodeValue()));
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

	public Documentation(Document docIn, String documentationFileName, LSParser lsParser, LSInput lsInput) {
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
			lsInput.setByteStream(new FileInputStream(new File(documentationFileName)));
			documentationDoc = lsParser.parse(lsInput);
		} catch (Exception e) {
			System.out.println("Got Exception while creating XML document: "
					+ e);
		}
		NodeList childList = documentationDoc.getDocumentElement().getChildNodes();
		for(int i = 0; i < childList.getLength(); ++i) {
			documentations.add(new DocumentationElement(childList.item(i)));
		}
	}

	public void getDocumentation(Node n) {
		if(contains(n) != -1) {
			System.out.println("Has documentation");
		} else {
			System.out.println("Doesn't have documentation");
		}
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
