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
package ModelInterface;

import javax.swing.JDialog;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.BoxLayout;
import javax.swing.Box;
import javax.swing.BorderFactory;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Frame;
import java.awt.Container;
import java.awt.BorderLayout;

import ModelInterface.InterfaceMain.MenuManager;

/**
 * Class which simply displays an about dialog including the license text.
 * This class implements the MenuAdder interface in order to add and handle
 * events for when the About Menu is clicked.
 * 
 * @author Pralit Patel
 */
public class AboutDialog extends JDialog implements MenuAdder, ActionListener {
    /**
     * Position in the Help submenu where the menu for About will go.
     * Note this is a suggested position for which the MenuManager will need
     * to sort out.
     */
	public static final int HELP_ABOUT_MENUITEM_POS = 1;

    /**
     * The button label/action command which indicates to hide the dialog.
     */
	public static final String CLOSE_COMMAND = "Close";

    /**
     * The license text which must be included in this dialog.
     */
	public static final String LICENSE_TEXT = "<html><body><br>LEGAL NOTICE<br>This computer software was prepared by Battelle Memorial Institute,<br>hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830<br>with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE<br>CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY<br>LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this<br>sentence must appear on any copies of this computer software.<br><br>Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.<br>Distributed as open-source under the terms of the Educational Community <br>License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php<br><br>EXPORT CONTROL<br>User agrees that the Software will not be shipped, transferred or<br>exported into any country or used in any manner prohibited by the<br>United States Export Administration Act or any other applicable<br>export laws, restrictions or regulations (collectively the \"Export Laws\").<br>Export of the Software may require some form of license or other<br>authority from the U.S. Government, and failure to obtain such<br>export control license may result in criminal liability under<br>U.S. laws. In addition, if the Software is identified as export controlled<br>items under the Export Laws, User represents and warrants that User<br>is not a citizen, or otherwise located within, an embargoed nation<br>(including without limitation Iran, Syria, Sudan, Cuba, and North Korea)<br>and that User is not otherwise prohibited<br>under the Export Laws from receiving the Software.<br><br>Please check <i>Additional Licenses</i> folder for third party licenses.</body></html>";

    /**
     * Constructor which forwards to the super class and sets the text to
     * display in the About dialog.
     */
    public AboutDialog(Frame parentFrame) {
        super(parentFrame, "About");

		JPanel all = new JPanel();
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		JLabel label1 = new JLabel("A tool to create inputs and view outputs for the GCAM model.");
		JLabel label2 = new JLabel("Use of this tool is subject to the following terms:");
		JLabel licLabel = new JLabel(LICENSE_TEXT);
        JButton closeButton = new JButton(CLOSE_COMMAND);
        closeButton.addActionListener(this);
		Container contentPane = getContentPane();
		all.add(label1, BorderLayout.PAGE_START);
        all.add(label2);
        all.add(licLabel);
		all.add(Box.createVerticalStrut(10));
		all.add(closeButton);
		all.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(all, BorderLayout.PAGE_START);
		pack();
    }

	/**
	 * Add menu items to the menu manager is specific locations.
	 * 
	 * @param aMenuManager
	 *            The menu manager to which to add menus and menu items.
	 */
	public void addMenuItems(MenuManager aMenuManager) {
		JMenuItem aboutMenu = new JMenuItem("About");
        aboutMenu.setEnabled(true);
        aboutMenu.addActionListener(this);
		aMenuManager.getSubMenuManager(InterfaceMain.HELP_MENU_POS)
            .addMenuItem(aboutMenu, HELP_ABOUT_MENUITEM_POS);
    }

    /**
     * Handle events including when the menu item is clicked (show this dialog)
     * or the ok button is clicked(hide this dialog).
     * @param e The even which needs to be hanlded.
     */
    public void actionPerformed(ActionEvent e) {
        if(e.getActionCommand() == CLOSE_COMMAND) {
            setVisible(false);
        } else {
            setVisible(true);
        }
    }
}
