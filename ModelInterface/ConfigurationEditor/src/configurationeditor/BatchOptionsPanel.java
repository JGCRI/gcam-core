/**
 * 
 */
package ModelInterface.ConfigurationEditor.src.configurationeditor;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;

import ModelInterface.ConfigurationEditor.src.guicomponents.DOMButtonModel;
import ModelInterface.ConfigurationEditor.src.guicomponents.DOMTextFieldFactory;
import ModelInterface.ConfigurationEditor.src.guihelpers.FieldButtonEnabler;
import ModelInterface.ConfigurationEditor.src.guihelpers.XMLFileFilter;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.BevelBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ModelInterface.ConfigurationEditor.src.utils.FileUtils;
import ModelInterface.ConfigurationEditor.src.utils.Messages;

/**
 * @author Josh Lurz
 * An options panel which contains information for editing the 
 * batch file settings.
 */
public class BatchOptionsPanel extends AbstractEditorPanel {

    /**
     * Unique identifier for serializing.
     */
    private static final long serialVersionUID = -3437736790254135793L;
    
    /**
     * Constructor
     */
    public BatchOptionsPanel() {
        super();
        initialize();
    }
    /**
     * This method initializes the batch editor panel.
     */
    private void initialize() {
        setBorder(BorderFactory
                .createBevelBorder(BevelBorder.RAISED));
        setToolTipText(Messages.getString("ConfigurationEditor.138")); //$NON-NLS-1$
        // Create all the batch file elements so the listener
        // can access them.
        final JLabel label = new JLabel(Messages
                .getString("ConfigurationEditor.124")); //$NON-NLS-1$

        final JTextField fileField = createBatchFileField();
        final JButton editButton = createBatchFileEditButton(fileField);
        // Add an action listener to enable the edit button. This
        // has to be done outside the create method to avoid a cycle.
        fileField.getDocument().addDocumentListener(
                new FieldButtonEnabler(editButton));
        final JButton selectButton = createBatchFileSelectButton(fileField);
        final JButton newButton = createBatchFileNewButton(fileField);
        final JCheckBox doBatchMode = createBatchCheckBox();

        // Add a listener which will enable and disable the batch
        // mode fields.
        doBatchMode.addChangeListener(new ChangeListener() {
            public void stateChanged(final ChangeEvent aEvent) {
                // Enable the fields according to the value of the checkbox.
                final boolean enabled = doBatchMode.isSelected();
                label.setEnabled(enabled);
                fileField.setEnabled(enabled);
                selectButton.setEnabled(enabled);
                newButton.setEnabled(enabled);
                // Only enable the batch file edit button if a file name has
                // been
                // set.
                editButton.setEnabled(enabled
                        && fileField.getText().length() > 0);
            }

        });

        // Add the items.
        // TODO: Layout these.
        add(doBatchMode, null);
        add(label, null);
        add(fileField, null);
        add(selectButton, null);
        add(editButton, null);
        add(newButton, null);
    }
    
    /**
     * This method initializes the batch file text field.
     * 
     * @return The batch file text field.
     */
    private JTextField createBatchFileField() {
        // Create a text field factory and add it as a listener on the editor.
        final DOMTextFieldFactory textFieldFactory = new DOMTextFieldFactory();
        addPropertyChangeListener("document-replaced", textFieldFactory);
        final JTextField fileField = textFieldFactory.createTextField(
                "Files", "BatchFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        fileField.setPreferredSize(new Dimension(200, 20));
        fileField.setToolTipText(Messages.getString("ConfigurationEditor.141")); //$NON-NLS-1$
        return fileField;
    }

    /**
     * This method initializes the batch file select button.
     * 
     * @param aFileField
     *            The file field for which this button is selection.
     * @return The batch file select button.
     */
    private JButton createBatchFileSelectButton(final JTextField aFileField) {
        final JButton selectButton = new JButton(Messages
                .getString("ConfigurationEditor.142")); //$NON-NLS-1$
        selectButton.setToolTipText(Messages
                .getString("ConfigurationEditor.143")); //$NON-NLS-1$
        selectButton.setMnemonic(KeyEvent.VK_S);
        final Component parentWindow = this.getTopLevelAncestor();
        selectButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                final File selectedFile = FileUtils.selectFile(parentWindow,
                        new XMLFileFilter(), aFileField.getText(), false);
                if (selectedFile != null) {
                    // Set the text field.
                    aFileField.setText(selectedFile.getAbsolutePath());
                }
            }
        });
        return selectButton;
    }

    /**
     * This method initializes the new batch file button.
     * 
     * @param aTextField
     *            Text field in which to save the new file selected.
     * @return The new batch file button.
     */
    private JButton createBatchFileNewButton(final JTextField aTextField) {
        final JButton newButton = new JButton(Messages
                .getString("ConfigurationEditor.145")); //$NON-NLS-1$
        newButton.setToolTipText(Messages.getString("ConfigurationEditor.146")); //$NON-NLS-1$
        newButton.setMnemonic(KeyEvent.VK_N);

        final Component parentWindow = this.getTopLevelAncestor();
        // Create an input field, dispatch the editor.
        newButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                final File selectedFile = FileUtils.selectFile(parentWindow,
                        new XMLFileFilter(), aTextField.getText(), true);
                if(selectedFile != null){
                    aTextField.setText(selectedFile.getAbsolutePath());
                    SwingUtilities.invokeLater(new BatchEditorCreator(selectedFile,
                            true));
                }
            }
        });
        return newButton;
    }

    /**
     * This method initializes the batch file edit button.
     * 
     * @param aTextField
     *            Text field in which to save the new file selected.
     * @return The batch file edit button.
     */
    private JButton createBatchFileEditButton(final JTextField aTextField) {
        final JButton editButton = new JButton(Messages
                .getString("ConfigurationEditor.149")); //$NON-NLS-1$
        editButton
                .setToolTipText(Messages.getString("ConfigurationEditor.150")); //$NON-NLS-1$
        editButton.setMnemonic(KeyEvent.VK_E);

        editButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                // Create a new batch file editor.
                final File batchFile = new File(aTextField.getText());
                SwingUtilities.invokeLater(new BatchEditorCreator(batchFile,
                        false));
            }
        });
        return editButton;
    }
    
    /**
     * This method initializes the do batch mode check box.
     * 
     * @return The batch mode checkbox.
     */
    private JCheckBox createBatchCheckBox() {
        final JCheckBox batchCheckBox = new JCheckBox(Messages
                .getString("ConfigurationEditor.16")); //$NON-NLS-1$

        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        final DOMButtonModel model = new DOMButtonModel(null, parentXPath,
                ConfigurationEditor.ELEMENT_NAME, "BatchMode", false);
        batchCheckBox.setModel(model); //$NON-NLS-1$
        // Add the checkbox as a listener so it will receive events
        // when the document is replaced.
        addPropertyChangeListener("document-replaced", model);
        batchCheckBox.setMnemonic(KeyEvent.VK_B);
        batchCheckBox.setToolTipText(Messages
                .getString("ConfigurationEditor.17")); //$NON-NLS-1$

        return batchCheckBox;
    }
}
