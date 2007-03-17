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

