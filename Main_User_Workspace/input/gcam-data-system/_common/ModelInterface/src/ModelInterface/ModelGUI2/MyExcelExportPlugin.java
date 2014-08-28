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

import ModelInterface.ModelGUI2.tables.BaseTableModel;

import org.jfree.report.modules.gui.xls.ExcelExportPlugin;
import org.jfree.report.modules.gui.xls.ExcelExportDialog;
//import org.jfree.report.modules.gui.base.components.AbstractExportDialog;
import org.jfree.report.modules.gui.base.ExportTask;
import org.jfree.report.JFreeReport;
import org.jfree.report.modules.gui.base.PreviewProxy;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.File;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import javax.swing.JOptionPane;
import java.awt.Dialog;
import java.awt.Frame;
/*
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import org.jfree.ui.action.ActionButton;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
*/

public class MyExcelExportPlugin extends ExcelExportPlugin {
	public static String enableKey = "org.jgcri.ModelGUI2.xls.Enable";
	public static BaseTableModel bt;
	public MyExcelExportPlugin() {
		super();
	}
	public boolean performExport( final JFreeReport report ) {
		final ExcelExportDialog exportDialog = getExportDialog();
		if (exportDialog.performQueryForExport(report) == false) {
			// user canceled the dialog ...
			return handleExportResult(true);
		}
		final ExportTask task = (new ExportTask() {
			protected void performExport() {
				try {
					exportToExcel(exportDialog.getFilename());
					setTaskDone();
				} catch( Exception e) {
					e.printStackTrace();
					setTaskFailed(e);
				}
			}
		});
		task.addExportTaskListener(new DefaultExportTaskListener());
		delegateTask(task);
		return handleExportResult(task);
	}
	protected ExcelExportDialog getExportDialog() {
		ExcelExportDialog exportDialog = null;
		if(exportDialog == null) {
			final PreviewProxy proxy = super.getProxy();
			if(proxy instanceof Frame) {
				exportDialog = new MyExcelExportDialog((Frame)proxy);
			} else if(proxy instanceof Dialog) {
				exportDialog = new MyExcelExportDialog((Dialog)proxy);
			} else {
				exportDialog = new MyExcelExportDialog();
			}
			exportDialog.pack();
		}
		return exportDialog;
	}
	protected class MyExcelExportDialog extends ExcelExportDialog {
		public MyExcelExportDialog() {
			super();
		}
		public MyExcelExportDialog(Frame pf) {
			super(pf);
		}
		public MyExcelExportDialog(Dialog pd) {
			super(pd);
		}
		protected boolean performConfirm() {
			final File f = new File(getFilename());
			if(f.exists()) {
				Object[] opts = {"Overwrite", "Append", "Cancel"};
				JOptionPane existsErrMess = new JOptionPane("The Selected File Exists", JOptionPane.WARNING_MESSAGE, 
						JOptionPane.YES_NO_CANCEL_OPTION, null, opts);
				existsErrMess.createDialog(this, "File Exists Error").setVisible(true);
				if(existsErrMess.getValue().equals(opts[0])) {
					f.delete();
				} else if(existsErrMess.getValue().equals(opts[2])) {
					return false;
				}
			}
			return true;
		}
	}
	protected void exportToExcel(String fileName) throws Exception {
		HSSFWorkbook wb = null;
		HSSFSheet sheet = null;
		File file = new File(fileName);
		if(file.exists()) {
			wb = new HSSFWorkbook(new FileInputStream(file));
			//sheet = wb.getSheetAt(0);
		}
		if(wb == null) {
			wb = new HSSFWorkbook();
		}
		sheet = wb.createSheet("Sheet"+String.valueOf(wb.getNumberOfSheets()+1));
		if(sheet == null) {
			System.out.println("The sheet is null");
		}
		bt.exportToExcel(sheet, wb, sheet.createDrawingPatriarch());
		FileOutputStream fos = new FileOutputStream(file);
		wb.write(fos);
		fos.close();
	}
}
