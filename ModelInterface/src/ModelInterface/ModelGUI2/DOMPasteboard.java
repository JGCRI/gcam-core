package ModelInterface.ModelGUI2;
import org.w3c.dom.*;
import org.w3c.dom.ls.DOMImplementationLS;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.Toolkit;
import javax.swing.JTree;
import ModelInterface.ModelGUI2.DOMmodel.DOMNodeAdapter;
import javax.swing.KeyStroke;
import javax.swing.JComponent;
import java.awt.datatransfer.*;

public class DOMPasteboard implements ActionListener {
	private Document doc;
	private Clipboard clipboard;
	private JTree jtree;
	public static int DOM_COPY = 0;
	public static int DOM_CUT = 1;
	public static int DOM_PASTE = 2;
	private Node buf;
	private int lastCmd;
	private DOMImplementationLS implls;
	public DOMPasteboard(Document docIn, JTree jtreeIn, DOMImplementationLS impllsIn) {
		doc = docIn;
		implls = impllsIn;
		jtree = jtreeIn;
		buf = null;
		lastCmd = -1;
		//or something like that look at copy paste
		//jtree.addActionListern(this);
		// copy keystroke is Control - C
		KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK,false);
		// copy keystroke is Control - V
		KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,ActionEvent.CTRL_MASK,false);
		jtree.registerKeyboardAction(this,"Copy",copy,JComponent.WHEN_FOCUSED);
		jtree.registerKeyboardAction(this,"Paste",paste,JComponent.WHEN_FOCUSED);
		clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	}
	public void actionPerformed(ActionEvent e) {
		System.out.println("Gets Called");
		if(e.getActionCommand().equals("Copy")) {
			buf = ((DOMNodeAdapter)((JTree)e.getSource()).getSelectionPath().getLastPathComponent()).getNode();
			putNodeInClipboard(buf);
			lastCmd = DOM_COPY;
		} else if(e.getActionCommand().equals("Cut")) {
			//buf = (Node)e.getSource();
			buf = ((DOMNodeAdapter)((JTree)e.getSource()).getSelectionPath().getLastPathComponent()).getNode();
			lastCmd = DOM_CUT;
		} else if(e.getActionCommand().equals("Paste")) {
			if(lastCmd == -1) {
				// nothing to past
				return;
			} else if(lastCmd == DOM_COPY) {
				//Node parent = (Node)e.getSource();
				Node parent = ((DOMNodeAdapter)((JTree)e.getSource())
						.getSelectionPath().getLastPathComponent()).getNode();
				parent.appendChild(buf.cloneNode(true));
			} else if(lastCmd == DOM_CUT) {
				//Node parent = (Node)e.getSource();
				Node parent = ((DOMNodeAdapter)((JTree)e.getSource())
						.getSelectionPath().getLastPathComponent()).getNode();
				buf.getParentNode().removeChild(buf);
				parent.appendChild(buf);
				lastCmd = DOM_COPY;
			}
		}
	}
	public boolean canCopy(Node n) {
		return true;
	}
	public boolean canCut(Node n) {
		return n != doc.getDocumentElement();
	}
	public boolean canPaste(Node n) {
		return lastCmd != -1;
	}
	private void putNodeInClipboard(Node buf) {
		TransferableXML xmlContent = new TransferableXML(implls.createLSSerializer().writeToString(buf));
		//System.out.println("Content Should be: "+implls.createLSSerializer().writeToString(buf));
		clipboard.setContents(xmlContent, xmlContent);
	}

	private class TransferableXML implements Transferable, ClipboardOwner {
		private String xmlData;
		private DataFlavor[] transFlavors;
		public TransferableXML(String xmlDataIn) {
			xmlData = xmlDataIn;
			transFlavors = new DataFlavor[1];
			transFlavors[0] = DataFlavor.stringFlavor;
		}
		public DataFlavor[] getTransferDataFlavors() {
			return transFlavors;
		}
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return flavor.isFlavorTextType();
		}
		public Object getTransferData(DataFlavor flavor) {
			if(isDataFlavorSupported(flavor)) {
				return xmlData;
			} else {
				return null;
			}
		}
		public void lostOwnership(Clipboard cp, Transferable contents) {
			// what to do here?
		}
	}
}

