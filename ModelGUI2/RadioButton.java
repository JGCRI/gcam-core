import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/*
 * ListDialog.java is a 1.4 class meant to be used by programs such as
 * ListDialogRunner.  It requires no additional files.
 */

public class RadioButton extends JDialog
						implements ActionListener {
	private static RadioButton dialog;
	private static String value = "";
	private JList list;

	public static String showDialog(Component frameComp,
									Component locationComp,
									String labelText,
									String title,
									String[] possibleValues,
									String initialValue) {
		Frame frame = JOptionPane.getFrameForComponent(frameComp);
		dialog = new RadioButton(frame,
								locationComp,
								labelText,
								title,
								possibleValues,
								initialValue);
		dialog.setVisible(true);
		return value;
	}

	private RadioButton(Frame frame,
					   Component locationComp,
					   String labelText,
					   String title,
					   Object[] data,
					   String initialValue) {
		super(frame, title, true);

		//Create and initialize the buttons.
		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		//
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
		JLabel label = new JLabel(labelText);
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
		setLocationRelativeTo(locationComp);
	}

	//Handle clicks on the Set and Cancel buttons.
	public void actionPerformed(ActionEvent e) {
		if ("Select".equals(e.getActionCommand())) {
			RadioButton.value = (String)(list.getSelectedValue());
			System.out.println("button selected!!!!! it's " + list.getSelectedValue().toString());
		}
		RadioButton.dialog.setVisible(false);
	}
}

