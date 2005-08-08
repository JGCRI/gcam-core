//package ModelGUI2;
package ModelInterface.ModelGUI2;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.ls.*;

public class ParseFilter implements LSParserFilter {
	/**
	 * Determines which Nodes are wanted in our document, the empty textnodes are
	 * not wanted.
	 * @param nodeArg current node being analyzed.
	 * @return flag indicating wheter the node was accepted or rejected
	 */
	public short acceptNode(Node nodeArg) {
		if (nodeArg.getNodeValue().matches("\\s*")) {
			return FILTER_REJECT;
		}
		return FILTER_ACCEPT;
	}

	/**
	 * Determines which types of Nodes should be checked as to whether they
	 * should be filtered out.
	 * @return flag for text nodes since we only want to analyze text nodes
	 */
	public int getWhatToShow() {
		return NodeFilter.SHOW_TEXT;
	}

	/**
	 * Called when the start tags for each element is read.
	 * @param elemtnArg the new tag being analyzed
	 * @return since we only filter empty text nodes we always return accept flag
	 */ 
	public short startElement(Element elementArg) {
		return FILTER_ACCEPT;
	}
}
