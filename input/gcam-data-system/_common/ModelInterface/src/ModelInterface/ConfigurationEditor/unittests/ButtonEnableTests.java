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
package ModelInterface.ConfigurationEditor.unittests;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;
import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.TestHelper;
import junit.extensions.jfcunit.finder.JMenuItemFinder;
import junit.extensions.jfcunit.finder.NamedComponentFinder;

/**
 * Units tests for enabled and disabled button in the main window.
 * 
 * @author Josh Lurz
 */
public class ButtonEnableTests extends JFCTestCase {

    /**
     * Configuration editor instance to test.
     */
    ConfigurationEditor mEditorInstance = null;

    /**
     * Constructor for ButtonEnableTests.
     * 
     * @param name
     */
    public ButtonEnableTests(String name) {
        super(name);
    }

    /*
     * @see TestCase#setUp()
     */
    @Override
    protected void setUp() throws Exception {
        super.setUp();

        // Choose the text Helper
        setHelper(new JFCTestHelper()); // Uses the AWT Event Queue.

        // Create the Configuration editor to test.
        mEditorInstance = new ConfigurationEditor();
        mEditorInstance.pack();
        mEditorInstance.setVisible(true);

    }

    /*
     * @see TestCase#tearDown()
     */
    @Override
    protected void tearDown() throws Exception {
        mEditorInstance = null;
        TestHelper.cleanUp(this);
        super.tearDown();
    }

    /**
     * Test the state of the new button.
     */
    public void testNewButton() {
        final NamedComponentFinder finder = new NamedComponentFinder(
                JComponent.class, "NewDocumentButton");
        final JButton newButton = (JButton) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the new button", newButton);
        
        // Check if it is enabled
        assertTrue("New button was not enabled", newButton.isEnabled());
    }
    
    /**
     * Test the state of the new menu item.
     * 
     */
    public void testNewMenuItem() {
        // Find the file menu.
        JMenuItemFinder finder = new JMenuItemFinder("New");
        final JMenuItem newMenuItem = (JMenuItem)finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the new menu item", newMenuItem);
        
        // Ensure that it is enabled.
        assertTrue("New menu item is not enabled", newMenuItem.isEnabled());
    }
    
    /**
     * Test the state of the save button.
     * 
     */
    public void testSaveButton() {
        final NamedComponentFinder finder = new NamedComponentFinder(
                JComponent.class, "SaveDocumentButton");
        final JButton saveButton = (JButton) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the save button", saveButton);
        
        // Ensure that it is not enabled because a document is not loaded.
        assertFalse("Save button is enabled without a document", saveButton.isEnabled());
    }
    
    /**
     * Test the state of the save menu item.
     * 
     */
    public void testSaveMenuItem() {
        // Find the file menu.
        JMenuItemFinder finder = new JMenuItemFinder("Save");
        final JMenuItem saveMenuItem = (JMenuItem) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the save menu item", saveMenuItem);
        
        // Ensure that it is not enabled.
        assertFalse("Save menu item is enabled before a document is loaded", saveMenuItem.isEnabled());
    }
    
    /**
     * Test the state of the load button.
     * 
     */
    public void testLoadButton() {
        NamedComponentFinder finder = new NamedComponentFinder(
                JComponent.class, "LoadDocumentButton");
        final JButton loadButton = (JButton) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the save button", loadButton);
        
        // Ensure that it is enabled.
        assertTrue("Load button is not enabled", loadButton.isEnabled());
    }
    
    /**
     * Test the state of the new menu item.
     * 
     */
    public void testLoadMenuItem() {
        // Find the file menu.
        JMenuItemFinder finder = new JMenuItemFinder("Load");
        final JMenuItem loadMenuItem = (JMenuItem) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the load menu item", loadMenuItem);
        
        // Ensure that it is enabled.
        assertTrue("Load menu item is not enabled", loadMenuItem.isEnabled());
    }
    
    /**
     * Test the state of the run button.
     * 
     */
    public void testRunButton() {
        NamedComponentFinder finder = new NamedComponentFinder(
                JComponent.class, "RunModelButton");
        final JButton runButton = (JButton) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the run button", runButton);
        
        // Ensure that it is not enabled.
        assertFalse("Run button is enabled before a document was loaded", runButton.isEnabled());
    }
    
    /**
     * Test the state of the run menu item.
     * 
     */
    public void testRunMenuItem() {
        // Find the file menu.
        JMenuItemFinder finder = new JMenuItemFinder("Run");
        final JMenuItem runMenuItem = (JMenuItem) finder.find(mEditorInstance, 0);
        assertNotNull("Could not find the run menu item", runMenuItem);
        
        // Ensure that it is not enabled.
        assertFalse("Run menu item is enabled before a document was loaded", runMenuItem.isEnabled());
    }
}
