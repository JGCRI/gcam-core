import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.ls.*;

public class ParseFilter implements LSParserFilter {
	public short acceptNode(Node nodeArg) {
		if (nodeArg.getNodeValue().matches("\\s*")) {
			return FILTER_REJECT;
		}
		return FILTER_ACCEPT;
	}

	public int getWhatToShow() {
		return NodeFilter.SHOW_TEXT;
	}

	public short startElement(Element elementArg) {
		return FILTER_ACCEPT;
	}
}
