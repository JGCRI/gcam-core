package ModelInterface.ModelGUI2.tables;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.tree.TreePath;
import org.w3c.dom.Document;
import javax.swing.event.TableModelListener;

import ModelInterface.ModelGUI2.InputViewer;

public class TableSelector extends JDialog implements ActionListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//private static TableSelector dialog;
	private String value = "";
	private JList list;
	private static String[] data = { "Single Table", "Multi Tables", "Combo Tables" };
	//private static Frame parentFrame;

	/**
	 * Creates and shows a short dialog to choose which type of table the use would
	 * like to use.
	 * @return the name of the table selected
	 */
	private void showDialog() {
		setVisible(true);
	}
	/*
	public static String showDialog(Component frameComp,
									Component locationComp,
									String labelText,
									String title,
									String[] possibleValues,
									String initialValue) {
		Frame frame = JOptionPane.getFrameForComponent(frameComp);
		dialog = new TableSelector(frame,
								locationComp,
								labelText,
								title,
								possibleValues,
								initialValue);
		dialog.setVisible(true);
		return value;
	}
	*/

	/**
	 * Initializes some of the data and creates the list of selections, and sets up
	 * the layout.
	 */
	public TableSelector(Frame frame) {
		super(frame, "Table Types", true);

		//parentFrame = frame;
		//Create and initialize the buttons.
		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		final JButton setButton = new JButton("Select");
		setButton.setActionCommand("Select");
		setButton.addActionListener(this);
		getRootPane().setDefaultButton(setButton); // make this one default

		//main part of the dialog
		list = new JList(data);
		list.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		list.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					setButton.doClick(); //emulate button click
				}
			}
		});
		JScrollPane listScroller = new JScrollPane(list);
		listScroller.setPreferredSize(new Dimension(250, 80));
		listScroller.setMinimumSize(new Dimension(250, 80));
		listScroller.setAlignmentX(LEFT_ALIGNMENT);

		JPanel listPane = new JPanel();
		listPane.setLayout(new BoxLayout(listPane, BoxLayout.Y_AXIS));
		JLabel label = new JLabel("Choose Table Viewing Type");
		label.setLabelFor(list);
		listPane.add(label);
		listPane.add(Box.createRigidArea(new Dimension(0,5)));
		listPane.add(listScroller);
		listPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));

		//Lay out the buttons from left to right.
		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(cancelButton);
		buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
		buttonPane.add(setButton);

		//Put everything together, using the content pane's BorderLayout.
		Container contentPane = getContentPane();
		contentPane.add(listPane, BorderLayout.CENTER);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);

		pack();
		//setLocationRelativeTo(locationComp);
	}

	/**
	 * Handle clicks on the Select and Cancel buttons, sets the value to the name of
	 * the table that was selected.
	 * @param e the even that occured, only care about clicks on a button
	 */
	public void actionPerformed(ActionEvent e) {
		if ("Select".equals(e.getActionCommand())) {
			value = (String)(list.getSelectedValue());
		}
		setVisible(false);
	}
	/**
	 * Handles creating the table based on value, sets up column widths, and any 
	 * additional layers such as CopyPaste, and TableSorter.
	 * @param tp needed to create the table.
	 * @param doc needed to create the table.
	 * @param pf needed to create the table.
	 * @return Returns the pane to be displayed in the right side of the splitpane
	 */
	public JScrollPane createSelection(TreePath tp, Document doc, JFrame pf, InputViewer fcd) {
		showDialog();
		if(value.equals("")) {
			return null;
		}
		/*
	  	((FileChooserDemo)fcd).menuTableFilter.setEnabled(true);
		((FileChooserDemo)fcd).copyMenu.setEnabled(true);
		((FileChooserDemo)fcd).pasteMenu.setEnabled(true);
		// check to see if there were any previous listeners, if so
		// remove them from both copy and paste
		ActionListener[] actns = ((FileChooserDemo)fcd).copyMenu.getActionListeners();
		if(actns.length != 0) {
			((FileChooserDemo)fcd).copyMenu.removeActionListener(actns[0]);
			actns = ((FileChooserDemo)fcd).pasteMenu.getActionListeners();
			((FileChooserDemo)fcd).pasteMenu.removeActionListener(actns[0]);
		}
		*/
		if(value.equals("Single Table")) {
			BaseTableModel bt = new NewDataTableModel(tp, doc, pf, "Single Table");
			TableSorter sorter = new TableSorter(bt);
	  		//JTable jTable = new JTable(bt);
	  		JTable jTable = new JTable(sorter);

			// Should the listener be set like so..
	  		jTable.getModel().addTableModelListener((TableModelListener)fcd);
	  		sorter.setTableHeader(jTable.getTableHeader());

	  		jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

	  		javax.swing.table.TableColumn col;
	  		//Iterator i = regions.iterator();
		  	col = jTable.getColumnModel().getColumn(0);
		  	col.setPreferredWidth(75);
	  		int j = 1;
	  		while(j < jTable.getColumnCount()) {
		  		col = jTable.getColumnModel().getColumn(j);
		  		col.setPreferredWidth(jTable.getColumnName(j).length()*5+30);
		  		j++;
	  		}
			new CopyPaste( jTable );
			/*
			((FileChooserDemo)fcd).copyMenu.addActionListener(copyPaste);
			((FileChooserDemo)fcd).pasteMenu.addActionListener(copyPaste);
			*/
			return new JScrollPane(jTable);
		} else if(value.equals("Multi Tables")) {
			// disable the copy paste buttons becuase they try to copy/paste from all of
			// the tables in the multitable, need to figure out how to only do it from
			// the table that is in focus
			// using ctrl-c, ctrl-v still works
			/*
			((FileChooserDemo)fcd).copyMenu.setEnabled(false);
			((FileChooserDemo)fcd).pasteMenu.setEnabled(false);
			*/

			BaseTableModel bt = new MultiTableModel(tp, doc, pf, "Multi Tables");
			JTable jTable = new JTable(bt);
	  		jTable.getModel().addTableModelListener((TableModelListener)fcd);

			//jTable.setAutoResizeMode(JTABLE.AUTO_RESIZE_OFF);

			jTable.setCellSelectionEnabled(true);
			jTable.getColumnModel().getColumn(0).setCellRenderer(((MultiTableModel)bt).getCellRenderer(0,0));
			jTable.getColumnModel().getColumn(0).setCellEditor(((MultiTableModel)bt).getCellEditor(0,0));
			int j = 1;
			while( j < jTable.getRowCount()) {
				jTable.setRowHeight(j,200);
				j += 2;
			}
			//jTable.setRowHeight(200);
			new CopyPaste( jTable );
			return new JScrollPane(jTable);
		} else if(value.equals("Combo Tables")){
			BaseTableModel bt = new ComboTableModel(tp, doc, pf, "Combo Tables");
			TableSorter sorter = new TableSorter(bt);
			JTable jTable = new JTable(sorter);
			// Should the listener be set like so..
			jTable.getModel().addTableModelListener((TableModelListener)fcd);
	  		sorter.setTableHeader(jTable.getTableHeader());

			jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

			javax.swing.table.TableColumn col;
			int j = 0;
			while(j < jTable.getColumnCount()) {
				col = jTable.getColumnModel().getColumn(j);
				if(jTable.getColumnName(j).equals("")) {
					col.setPreferredWidth(75);
				} else {
					col.setPreferredWidth(jTable.getColumnName(j).length()*5+30);
				}
				j++;
			}
			new CopyPaste( jTable );
			/*
			((FileChooserDemo)fcd).copyMenu.addActionListener(copyPaste);
			((FileChooserDemo)fcd).pasteMenu.addActionListener(copyPaste);
			*/
			return new JScrollPane(jTable);		
		}
		return null;
	}
}
