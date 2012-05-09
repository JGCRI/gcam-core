/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ModelGUI2;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.DOMException;
import org.w3c.dom.ls.*;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;

import javax.swing.TransferHandler;
import javax.swing.JComponent;
import javax.swing.JTree;

import java.io.IOException;

import ModelInterface.ModelGUI2.DOMmodel.DOMNodeAdapter;

public class DOMTransferHandler extends TransferHandler {
	private Document doc;
	private DOMImplementationLS implls;
	private LSParser parser;
	public static DataFlavor DOM_NODE_DATAFLAVOR = new DataFlavor(Node.class, "Node"); // public?
	private int lastAction;

	public DOMTransferHandler(Document docIn, DOMImplementationLS impllsIn) {
		super();
		doc = docIn;
		implls = impllsIn;
		try {
			parser = implls.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS, null);
			parser.setFilter(new ParseFilter());
		} catch (DOMException de) {
			de.printStackTrace();
			// warn couldn't create parser ?
		}
	}
	public boolean importData(JComponent comp, Transferable t) {
		DataFlavor[] dfs = t.getTransferDataFlavors();
		boolean canString = false;
		Node curr = null;
		Node parent = ((DOMNodeAdapter)((JTree)comp)
				.getSelectionPath().getLastPathComponent()).getNode();
		String temp = null;
		for(DataFlavor df : dfs) {
			if(df.equals(DOM_NODE_DATAFLAVOR)) {
				try {
					curr = (Node)t.getTransferData(DOM_NODE_DATAFLAVOR);
					if(lastAction == MOVE) {
						parent.appendChild(curr);
					} else { 
						// copy action
						parent.appendChild(curr.cloneNode(true));
					}
					return true;
				} catch(IOException ioe) {
					ioe.printStackTrace();
					// warn data is no longer available as DOM Node
				} catch(UnsupportedFlavorException ufe) {
					ufe.printStackTrace();
					// warn that somehow DOM Node flavor isn't accepted
				} catch(DOMException de) {
					de.printStackTrace();
					// error couldn't append here, won't be able to paste return false
					return false;
				}
			} else if(df.equals(DataFlavor.stringFlavor)) {
				canString = true;
			}
		}
		if(canString) {
			try {
				temp = (String)t.getTransferData(DataFlavor.stringFlavor);
				LSInput tempInput = implls.createLSInput();
				tempInput.setStringData(temp);
				// doesn't seem to be supported
				//parser.parseWithContext(tempInput, parent, LSParser.ACTION_APPEND_AS_CHILDREN);
				curr = doc.adoptNode(parser.parse(tempInput).getDocumentElement());
				parent.appendChild(curr);
				return true;
			} catch(IOException ioe) {
				ioe.printStackTrace();
				// warn data is no longer available as DOM Node
			} catch(UnsupportedFlavorException ufe) {
				ufe.printStackTrace();
				// warn that somehow DOM Node flavor isn't accepted
			} catch(DOMException de) {
				de.printStackTrace();
				// warn couldn't parse, might not have been XML
				// try to append as text node
				return attemptTextNodePaste(parent, temp);
			} catch(LSException le) {
				le.printStackTrace();
				return attemptTextNodePaste(parent, temp);
			}
		}
		return false;
		// do I need to tell the JTree to refresh itself?

	}
	public boolean canImport(JComponent comp, DataFlavor[] transFlav) {
		for (int i = 0; i < transFlav.length; i++) {
			if (DataFlavor.stringFlavor.equals(transFlav[i]) || 
					DOM_NODE_DATAFLAVOR.equals(transFlav[i])) {
				return true;
			}
		}
		return false;
	}
	protected Transferable createTransferable(JComponent comp) {
		return new TransferableXML(((DOMNodeAdapter)((JTree)comp).getSelectionPath().getLastPathComponent()).getNode());
	}
	protected void exportDone(JComponent comp, Transferable t, int action) {
		lastAction = action;
		if(action == MOVE) {
			((JTree)comp).removeSelectionPaths(((JTree)comp).getSelectionPaths());
			// do I need to remove from DOM, is it going to delete it?
			// don't want it to be deleted to I?
		}
	}
	public int getSourceActions(JComponent c) {
		return COPY_OR_MOVE;
	}

	private boolean attemptTextNodePaste(Node parent, String txt) {
		try {
			Node txtNode = doc.createTextNode(txt);
			parent.appendChild(txtNode);
			return true;
		} catch (DOMException de2) {
			de2.printStackTrace();
			// error couldn't append here, won't be able to paste return false
			return false;
		}
	}

	private class TransferableXML implements Transferable {
		private Node curr;
		private DataFlavor[] transFlavors;
		public TransferableXML(Node currIn) {
			curr = currIn;
			transFlavors = new DataFlavor[2];
			transFlavors[0] = DOM_NODE_DATAFLAVOR;
			transFlavors[1] = DataFlavor.stringFlavor;
		}
		public DataFlavor[] getTransferDataFlavors() {
			return transFlavors;
		}
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return flavor.isFlavorTextType() || DOM_NODE_DATAFLAVOR.equals(flavor);
		}
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
			if(DOM_NODE_DATAFLAVOR.equals(flavor)) {
				return curr;
			} else if(flavor.isFlavorTextType()) {
				return implls.createLSSerializer().writeToString(curr);
			} else {
				throw new UnsupportedFlavorException(flavor);
			}
		}
	}
}
