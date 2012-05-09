/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
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
package ModelInterface.ModelGUI2.tables;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;

import javax.swing.TransferHandler;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTabbedPane;

import java.io.IOException;

import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.DbViewer;

public class TableTransferHandler extends TransferHandler {
	public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
		return false;
	}
	public int getSourceActions(JComponent c) {
		return TransferHandler.COPY;
	}
	public boolean importData(JComponent comp, Transferable t) {
		return false;
	}
	protected Transferable createTransferable(JComponent comp) {
		return new TransferableTable(comp);
	}
	private class TransferableTable implements Transferable {
		private BaseTableModel bt;
		private DataFlavor[] transFlavors;
		public TransferableTable(JComponent comp) {
			if(comp instanceof JTabbedPane) {
				bt = DbViewer.getTableModelFromComponent(((JTabbedPane)comp).getSelectedComponent());
			} else if(comp instanceof JTable) {
				bt = (BaseTableModel)((JTable)comp).getModel();
			} else {
				throw new UnsupportedOperationException("Can't transfer this component");
			}
			transFlavors = new DataFlavor[1];
			transFlavors[0] = DataFlavor.stringFlavor;
		}
		public DataFlavor[] getTransferDataFlavors() {
			return transFlavors;
		}
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			for(DataFlavor tranFlavor : transFlavors) {
				if(tranFlavor.equals(flavor)) {
					return true;
				}
			}
			return false;
		}
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
			if(!isDataFlavorSupported(flavor)) {
				throw new UnsupportedFlavorException(flavor);
			}
			// only string for now..
			return bt.exportToText();
		}
	}
}

