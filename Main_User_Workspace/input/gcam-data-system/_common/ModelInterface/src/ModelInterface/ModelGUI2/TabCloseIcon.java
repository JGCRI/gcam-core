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
package ModelInterface.ModelGUI2;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTabbedPane;

import ModelInterface.InterfaceMain;

public class TabCloseIcon implements Icon {

	private static final Icon closeIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/closeTab.PNG"));
	//mOverCloseIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/mOverCloseTab.PNG"));
	private static final Icon mPressCloseIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/mPressCloseTab.PNG"));
	//private final Icon mOverCloseIcon;
	private static final Icon loadingIcon = new ImageIcon( TabCloseIcon.class.getResource("icons/loadTab.PNG"));
	private JTabbedPane tabPane = null;
	private Icon showingIcon;
	private transient Rectangle position = null;
	
	public TabCloseIcon(JTabbedPane tabPaneIn) {
		showingIcon = loadingIcon;
		tabPane = tabPaneIn;
		tabPane.addMouseListener(new MouseAdapter() {
			@Override public void mouseReleased( MouseEvent e ) {
				// asking for isConsumed is *very* important, otherwise more than one tab might get closed!
				if ( !e.isConsumed()  &&   position.contains( e.getX(), e.getY() ) ) {
					final int index = tabPane.indexAtLocation(e.getX(), e.getY());
					QueryResultsPanel closeThread = (QueryResultsPanel)(tabPane.getComponentAt(index));
					closeThread.killThread();
					InterfaceMain.getInstance().fireProperty("Query", 
						DbViewer.getTableModelFromComponent(closeThread), null);
					tabPane.removeMouseListener(this);
					tabPane.remove( index );
					e.consume();
				}
			}
			/*
			   @Override public void mouseEntered( MouseEvent e ) {
			   System.out.println("Entered Called");
			   System.out.println(position);
			   System.out.println("("+e.getX()+", "+e.getY()+")");
			   if ( !e.isConsumed()  &&   position.contains( e.getX(), e.getY() ) ) {
			   System.out.println("Entered IF");
			   showingIcon = mOverCloseIcon;
			   //showingIcon.paintIcon(c, g, x, y );
			   }
			   }
			   */
			@Override public void mouseExited( MouseEvent e ) {
				if ( !e.isConsumed() /* &&   position.contains( e.getX(), e.getY() )*/ ) {
					showingIcon = closeIcon;
					//showingIcon.paintIcon(c, g, x, y );
				}
			}
			@Override public void mousePressed( MouseEvent e ) {
				/*
				   System.out.println("Pressed Called");
				   System.out.println(position);
				   System.out.println("("+e.getX()+", "+e.getY()+")");
				   */
				if ( !e.isConsumed()  &&   position.contains( e.getX(), e.getY() ) ) {
					//System.out.println("Pressed IF");
					showingIcon = mPressCloseIcon;
					//showingIcon.paintIcon(c, g, x, y );
				}
			}
		});
	}
	
	public void finishedLoading() {
		showingIcon = closeIcon;
		tabPane.repaint();
	}
	
	
	public void paintIcon(Component c, Graphics g, int x, int y) {
		position = new Rectangle( x,y, getIconWidth(), getIconHeight() );
		showingIcon.paintIcon(c, g, x, y );
	}
	
	public int getIconWidth() {
		return showingIcon.getIconWidth();
	}
	
	public int getIconHeight() {
		return showingIcon.getIconHeight();
	}
}
