package ModelInterface.ModelGUI2.tables;

import ModelInterface.InterfaceMain;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.awt.datatransfer.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

public class CopyPaste implements ActionListener{
	private Clipboard clipboard;
	private StringSelection stringSelection;
	private JTable myJTable ;

	/**
	 * Constructor initializes the table for which we will be copy and pasting
	 * @param the table for which we will be copy and pasting
	 */
	public CopyPaste(JTable jTable)
    {
	  myJTable = jTable;
	  // copy keystroke is Control - C
	  KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK,false);
	  // copy keystroke is Control - V
	  KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,ActionEvent.CTRL_MASK,false);
	  myJTable.registerKeyboardAction(this,"Copy",copy,JComponent.WHEN_FOCUSED);
	  myJTable.registerKeyboardAction(this,"Paste",paste,JComponent.WHEN_FOCUSED);
	  clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	  final JMenuItem copyMenu = InterfaceMain.getInstance().getCopyMenu();
	  final JMenuItem pasteMenu = InterfaceMain.getInstance().getPasteMenu();
	  copyMenu.addActionListener(this);
	  copyMenu.setEnabled(true);
	  pasteMenu.addActionListener(this);
	  // need to make this false if it is db
	  // only way i can think of right now
	  // is to check the xmlDB
	  pasteMenu.setEnabled(ModelInterface.ModelGUI2.DbViewer.xmlDB == null);
	  final CopyPaste thisCP = this;
	  InterfaceMain.getInstance().addPropertyChangeListener(new PropertyChangeListener() {
		  public void propertyChange(PropertyChangeEvent e) {
			  if(e.getPropertyName().equals("Control")) {
				  System.out.println("Doing remove because of control");
				  copyMenu.setEnabled(false);
				  copyMenu.removeActionListener(thisCP);
				  pasteMenu.setEnabled(false);
				  pasteMenu.removeActionListener(thisCP);
				  InterfaceMain.getInstance().removePropertyChangeListener(this);
			  } else if(( e.getPropertyName().equals("Query") || e.getPropertyName().equals("Table")) && !e.getNewValue().equals(getMyModel())) {
				  /*
				  if((myJTable.getModel() instanceof TableSorter) && e.getNewValue() == ((TableSorter)myJTable.getModel()).getTableModel()) {
		  return;
				  }
				  */
				  System.out.println("New Table: "+e.getNewValue());
				  System.out.println("My Table: "+myJTable.getModel());
				  System.out.println("Doing remove because of query/table");
				  // don't want to disable these, since there is a new query/table which would
				  // have already enabled them for themselves
				  //copyMenu.setEnabled(false);
				  //pasteMenu.setEnabled(false);
				  copyMenu.removeActionListener(thisCP);
				  pasteMenu.removeActionListener(thisCP);
				  InterfaceMain.getInstance().removePropertyChangeListener(this);
			  }
		  }
	  });



    }
    private BaseTableModel getMyModel() {
	    if(myJTable.getModel() instanceof TableSorter) {
		    return (BaseTableModel)((TableSorter)myJTable.getModel()).getTableModel();
	    } else {
		    return (BaseTableModel)myJTable.getModel();
	    }
    }

        /**
	 * Returns the table that the instance copys and pastes for
	 * @return this classes reference to it's jtable
	 */
	public JTable getJTable() {
		return myJTable;
	}

	/**
	 * Sets a new table to copy and paste for
	 * @param aJTable new JTable reference to copy paste to
	 */
	public void setJTable(JTable aJTable) {
		this.myJTable = aJTable;
	}

	/**
	 * Listen for copy and paste actions, and handle them appropriatly
	 * @param e the event that has occured, only care about copy and paste
	 */
	public void actionPerformed(ActionEvent e)
  	{
		if (e.getActionCommand().equals("Copy")) {
			/*
		if(!myJTable.hasFocus()) {
			System.out.println("Doesn't have focus");
			return;
		}
		*/
			// Make sure we have a contiguous block of cells
			int numRows=myJTable.getSelectedRowCount();
			if(numRows == 0) {
				System.out.println("Doesnt' have selection");
				return;
			}
			int numCols=myJTable.getSelectedColumnCount();
			StringBuffer stringBuffer = new StringBuffer();
			int[] rowsSelected=myJTable.getSelectedRows();
			int[] colsSelected=myJTable.getSelectedColumns();
			if (!((numRows-1==rowsSelected[rowsSelected.length-1]-rowsSelected[0] &&
				numRows==rowsSelected.length) && 
				(numCols-1==colsSelected[colsSelected.length-1]-colsSelected[0] &&
				numCols==colsSelected.length)))
			{
				JOptionPane.showMessageDialog(null, "Invalid Copy Selection",
											  "Invalid Copy Selection",
											  JOptionPane.ERROR_MESSAGE);
				return; //cancel copy
			}
			for (int i=0;i<numRows;i++){
				for (int j=0;j<numCols;j++){
					stringBuffer.append( myJTable.getValueAt( rowsSelected[i] ,colsSelected[j] ) );
				    if ( j != numCols-1 ){
				    	stringBuffer.append("\t"); 
				    }
				}
				stringBuffer.append("\n");
				//stringBuffer.append(System.getProperty("line.separator"));
			}
			stringSelection  = new StringSelection(stringBuffer.toString());
			clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
			clipboard.setContents(stringSelection,stringSelection);
		}
		  
		if (e.getActionCommand().equals("Paste")) {
		if(!myJTable.hasFocus()) {
			System.out.println("Doesn't have focus");
			return;
		}
			System.out.println("Time to paste ... ");
			String entireRow, onePiece;
			int startRow=(myJTable.getSelectedRows())[0];
			int startCol=(myJTable.getSelectedColumns())[0];
			try
			{
				String stringToPaste = (String)(clipboard.getContents(this).getTransferData(DataFlavor.stringFlavor));
				System.out.println("String is:" + stringToPaste);
				StringTokenizer stRow = new StringTokenizer(stringToPaste, "\n"); //divide into rows
				//StringTokenizer stRow = new StringTokenizer(stringToPaste, System.getProperty("line.separator")); //divide into rows
				for(int i=0; stRow.hasMoreTokens(); i++){	
					entireRow = stRow.nextToken();
					StringTokenizer stCol = new StringTokenizer(entireRow,"\t");
					for(int j=0; stCol.hasMoreTokens(); j++){
						onePiece = (String)stCol.nextToken();
						if (startRow+i < myJTable.getRowCount() && startCol+j < myJTable.getColumnCount()){
							String oldStr = myJTable.getValueAt( startRow+i, startCol+j ).toString();
							if ( areOfTheSameType( oldStr, onePiece )){
								myJTable.setValueAt(onePiece, startRow+i, startCol+j);
				  	 			System.out.println("Putting "+ onePiece + "at row =" + startRow+i + "column =" + startCol+j );
							}else{
								System.out.println("NOT of the same type: " + oldStr + " and " + onePiece);
							}
			   			 }
					}
				}
			}
		 	catch(Exception ex){
		 		ex.printStackTrace();
		 	}
		}
   	}
   	
	/**
	 * Make sure that the data being pasted is of a similar type, so that we don't have invalid data in
	 * out table and tree
	 * @param oldStr used to determine an appropriate type
	 * @param newStr new value to be checked if it is appropriate
	 * @return true if they are of similar types, false otherwise
	 */
   	public boolean areOfTheSameType( String oldStr, String newStr ){
   		try{
   			double oldDouble = Double.parseDouble(oldStr); //old is a double
   			try{
   				double newDouble = Double.parseDouble(newStr); //new is also an double :)
   				return true;
   			}catch(NumberFormatException nfe2){ // double, not double :(
   				return false;
   			}
   		}catch(NumberFormatException nfe1){ // oldStr is NOT an double, must be a string
			return true;
   		}
   		
   		/*
    		try{
   			int oldInt = Integer.parseInt(oldStr); //old is an int
   			try{
   				int newInt = Integer.parseInt(newStr); //new is also an int :)
   				return true;
   			}catch(NumberFormatException nfe2){ // int, notInt :(
   				return false;
   			}
   		}catch(NumberFormatException nfe1){ // oldStr is NOT an int
   			try{
   				double oldDouble = Double.parseDouble(oldStr); //old is a double
				try{
					double newDouble = Double.parseDouble(newStr); // both doubles :)
					return true;
				}catch(NumberFormatException nfe4){ //double, not double :(
					return false;	
				}
   			}catch(NumberFormatException nfe3){ // oldStr is just some string
   				return true; // i guess we can just return true
   			}
   		}
   		*/
   	}
}

