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
