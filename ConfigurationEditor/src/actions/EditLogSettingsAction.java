/**
 * 
 */
package actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.BevelBorder;

import configurationeditor.ConfigurationEditor;
import configurationeditor.Messages;

/**
 * This class displays a dialog for editing log settings.
 * @author Josh Lurz
 */
public class EditLogSettingsAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = 1257512799098654257L;

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private ConfigurationEditor mParentEditor = null;
	
    /**
	 * The dialog which contains the log editing UI.
	 */
	private JDialog mEditLogsDialog = null;
	
	private JPanel mLogPane = null;

	private JSplitPane mLogSplitPane = null;

	private JPanel mLogModificationPanel = null;

	private JComboBox mMinLogWarningLevel = null;

	private JLabel mLogWarningLevelLabel = null;

	private JPanel mLogSelecterPane = null;

	private JLabel mLogSelectorLabel = null;

	private JComboBox mLoggerSelector = null;

	private JLabel mLogToScreenLevelLabel = null;

	private JComboBox mMinLogToScreenWarningLevel = null;

	private JCheckBox mPrintLogWarningLevelCheckBox = null;
	
	/**
	 * Constructor
	 */
	public EditLogSettingsAction(ConfigurationEditor aParentEditor) {
		super("EditLogSettings"); //$NON-NLS-1$
		mParentEditor = aParentEditor;
	}

	/**
	 * Method called when an action is performed on the dispatching object.
	 * @param aEvent The event received.
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
    public void actionPerformed(ActionEvent aEvent) {
        JDialog editLogsDialog = getEditLogsDialog();
        editLogsDialog.pack();
        editLogsDialog.setVisible(true);
	}
	
	   /**
     * This method initializes preferenceDialogFrame
     * @return The edit logs dialog box.
     */    
    private JDialog getEditLogsDialog() {
        if (mEditLogsDialog == null) {
        	mEditLogsDialog = new JDialog(mParentEditor, Messages.getString("ShowPreferencesAction.3"), true ); //$NON-NLS-1$
        	mEditLogsDialog.setContentPane(getLogPane());
        }
        return mEditLogsDialog;
    }
    
    /**
     * This method initializes mLogPane
     * 
     * @return javax.swing.JPanel
     */
    private JPanel getLogPane() {
        if (mLogPane == null) {
            mLogPane = new JPanel();
            mLogPane.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
            mLogPane.setToolTipText(Messages.getString("ConfigurationEditor.80")); //$NON-NLS-1$
            mLogPane.add(getLogSplitPane(), null);
        }
        return mLogPane;
    }
    
    /**
     * This method initializes mLogSplitPane
     * 
     * @return javax.swing.JSplitPane
     */
    private JSplitPane getLogSplitPane() {
        if (mLogSplitPane == null) {
            mLogSplitPane = new JSplitPane();
            mLogSplitPane.setRightComponent(getLogModificationPanel());
            mLogSplitPane.setLeftComponent(getLogSelecterPane());
        }
        return mLogSplitPane;
    }
    
    /**
     * This method initializes mLogModificationPanel
     * 
     * @return javax.swing.JPanel
     */
    private JPanel getLogModificationPanel() {
        if (mLogModificationPanel == null) {
            mLogToScreenLevelLabel = new JLabel();
            mLogToScreenLevelLabel.setText(Messages.getString("ConfigurationEditor.81")); //$NON-NLS-1$
            mLogToScreenLevelLabel.setToolTipText(Messages.getString("ConfigurationEditor.83")); //$NON-NLS-1$
            mLogWarningLevelLabel = new JLabel();
            mLogWarningLevelLabel.setText(Messages.getString("ConfigurationEditor.84")); //$NON-NLS-1$
            mLogWarningLevelLabel
                    .setToolTipText(Messages.getString("ConfigurationEditor.86")); //$NON-NLS-1$
            mLogModificationPanel = new JPanel();
            mLogModificationPanel.setLayout(new BoxLayout(mLogModificationPanel,
                    BoxLayout.Y_AXIS));
            mLogModificationPanel.add(mLogWarningLevelLabel, null);
            mLogModificationPanel.add(getMinLogWarningLevel(), null);
            mLogModificationPanel.add(mLogToScreenLevelLabel, null);
            mLogModificationPanel.add(getMinLogToScreenWarningLevel(), null);
            mLogModificationPanel.add(getPrintLogWarningLevelCheckBox(), null);
        }
        return mLogModificationPanel;
    }

    /**
     * This method initializes mMinLogWarningLevel
     * 
     * @return javax.swing.JComboBox
     */
    private JComboBox getMinLogWarningLevel() {
        if (mMinLogWarningLevel == null) {
            mMinLogWarningLevel = new JComboBox();
            mMinLogWarningLevel.setSelectedIndex(-1);
            mMinLogWarningLevel
                    .setToolTipText(Messages.getString("ConfigurationEditor.87")); //$NON-NLS-1$
        }
        return mMinLogWarningLevel;
    }

    /**
     * This method initializes mLogSelecterPane
     * 
     * @return The log selection pane.
     */
    private JPanel getLogSelecterPane() {
        if (mLogSelecterPane == null) {
            mLogSelectorLabel = new JLabel();
            mLogSelectorLabel.setText(Messages.getString("ConfigurationEditor.89")); //$NON-NLS-1$
            mLogSelectorLabel.setToolTipText(Messages.getString("ConfigurationEditor.90")); //$NON-NLS-1$
            mLogSelecterPane = new JPanel();
            mLogSelecterPane.add(mLogSelectorLabel, null);
            mLogSelecterPane.add(getLoggerSelector(), null);
        }
        return mLogSelecterPane;
    }

    /**
     * This method initializes the logger selector.
     * 
     * @return The log selector combo box.
     */
    private JComboBox getLoggerSelector() {
        if (mLoggerSelector == null) {
            mLoggerSelector = new JComboBox();
        }
        return mLoggerSelector;
    }

    /**
     * This method initializes the minimum log to screen combo box.
     * 
     * @return The minimum log to screen combo box.
     */
    private JComboBox getMinLogToScreenWarningLevel() {
        if (mMinLogToScreenWarningLevel == null) {
            mMinLogToScreenWarningLevel = new JComboBox();
            mMinLogToScreenWarningLevel.setSelectedIndex(-1);
            mMinLogToScreenWarningLevel
                    .setToolTipText(Messages.getString("ConfigurationEditor.92")); //$NON-NLS-1$
        }
        return mMinLogToScreenWarningLevel;
    }

    /**
     * This method initializes the print log warning level checkbox.
     * 
     * @return The print log warning level checkbox.
     */
    private JCheckBox getPrintLogWarningLevelCheckBox() {
        if (mPrintLogWarningLevelCheckBox == null) {
            mPrintLogWarningLevelCheckBox = new JCheckBox();
            mPrintLogWarningLevelCheckBox.setMnemonic(KeyEvent.VK_W);
            mPrintLogWarningLevelCheckBox
                    .setText(Messages.getString("ConfigurationEditor.94")); //$NON-NLS-1$
            mPrintLogWarningLevelCheckBox
                    .setToolTipText(Messages.getString("ConfigurationEditor.95")); //$NON-NLS-1$
        }
        return mPrintLogWarningLevelCheckBox;
    }

}
