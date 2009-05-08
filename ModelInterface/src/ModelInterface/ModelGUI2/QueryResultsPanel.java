package ModelInterface.ModelGUI2;

import java.awt.Dimension;
import java.awt.image.BufferedImage;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;

import org.jfree.chart.JFreeChart;

import com.sleepycat.dbxml.XmlException;
import com.sleepycat.dbxml.XmlQueryContext;

import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.tables.BaseTableModel;
import ModelInterface.ModelGUI2.tables.ComboTableModel;
import ModelInterface.ModelGUI2.tables.CopyPaste;
import ModelInterface.ModelGUI2.tables.MultiTableModel;
import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.ModelGUI2.xmldb.XMLDB;

/**
 * Adds capability of running many queries parallel and will display
 * the results after the queries are run as well as change the icon 
 * to indicate to the user that it is done running.
 * 
 */

public class QueryResultsPanel extends JPanel {




	/** Referring to the thread that is running. Used to track
	 * which thread is being used/closed
	 */
	Thread runThread;

	/** The context for running queries which can be used to cancel it */
	XmlQueryContext context	= null;

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/**
	 * Instantiates a new query results panel.
	 * 
	 * @param qg the query generator
	 * @param singleBinding The single query binding which will filter the results from qg, or null if the user did not select a single query.
	 * @param parentFrame the frame that JPanel will be inserted into
	 * @param scenarioListValues list of selected scenarios
	 * @param regionListValues Regions to be used. 
	 * @param icon The icon that will be changing.
	 */
	public QueryResultsPanel(final QueryGenerator qg, final QueryBinding singleBinding, final JFrame parentFrame,final Object[] scenarioListValues, final Object[] regionListValues, final TabCloseIcon icon){  
		initializeWaiting();
		context = XMLDB.getInstance().createQueryContext();
		final QueryResultsPanel thisThread= this;
		runThread = new Thread(){
			public void run(){
				JComponent ret = null;
				String errorMessage = null;
				//do computations, return a JComponent
				try{
					if (qg.isGroup() && singleBinding == null) {
						ret = createGroupTableContent(qg, parentFrame, scenarioListValues, regionListValues);
					} else {
						ret = createSingleTableContent(qg, singleBinding, parentFrame, scenarioListValues, regionListValues);
					}
				} catch(Exception e) {
					errorMessage = e.getMessage();
				}
				//Stop process if the user terminated the process
				if(isInterrupted())
					return;

				//clear the text box in preparation of adding the new component 
				removeAll();

				//icon is changed to the finished state
				icon.finishedLoading();
				//error message displayed
				if(ret == null){
					JPanel tempPanel = new JPanel();
					tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
					tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
					tempPanel.add(new JLabel(errorMessage));
					add(tempPanel);
				}
				//the new JPanel is added where the text box was
				else{
					setLayout(new BoxLayout(thisThread, BoxLayout.X_AXIS));
					add(ret);
				}
				//the panel is refreshed to show the changes
				revalidate();
			}
		};
		runThread.start();
		System.gc();
	}

	//terminate the query process that is running
	/**
	 * Terminates the thread. This is normally run
	 * when the tab is closed before the simulation finishes.
	 */
	public void killThread(){
		try {
			context.interruptQuery();
		} catch (XmlException e) {
			
			e.printStackTrace();
		} finally {
			context.delete();
		}
		runThread.interrupt();
	}



	/**
	 * Inserts a text box that will be displayed until the results
	 * from the quert are available.
	 */
	public void initializeWaiting (){
		//write the text-box
		JPanel tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		tempPanel.add(new JLabel("Waiting for results. Close to terminate"));
		add(tempPanel);


	}




	/**
	 * Creates the group table content.
	 * 
	 * @param qg the query generator
	 * @param parentFrame the Frame that the JComponent will be added to
	 * @param scnListValues 
	 * @param regionListValues 
	 * 
	 * @return a JComponent with the results in it as a  JScrollingPane
	 * @throws Exception thrown if the multiTableModel returns an invalid result
	 * 
	 */
	private JComponent createGroupTableContent(QueryGenerator qg,final JFrame parentFrame,final Object[] scnListValues, final Object[] regionListValues) throws Exception {
		BaseTableModel bt = new MultiTableModel(qg, scnListValues, regionListValues, parentFrame, context);
		JTable jTable = new JTable(bt);
		jTable.setCellSelectionEnabled(true);
		jTable.getColumnModel().getColumn(0).setCellRenderer(((MultiTableModel)bt).getCellRenderer(0,0));
		jTable.getColumnModel().getColumn(0).setCellEditor(((MultiTableModel)bt).getCellEditor(0,0));
		((InterfaceMain)parentFrame).fireProperty("Query", null, bt);
		return new JScrollPane(jTable);
	}

	/**
	 * Creates the single table content.
	 * 
	 * @param qg the qg
	 * @param singleBinding 
	 * @param parentFrame the Frame that the JComponent will be added to
	 * @param scenarioListValues 
	 * @param regionListValues 
	 * 
	 * 
	 * @return a JComponent with the results in it as a  JScrollingPane
	 * @throws Exception thrown if the multiTableModel returns an invalid result
	 * 
	 */
	private JComponent createSingleTableContent(QueryGenerator qg, QueryBinding singleBinding, final JFrame parentFrame,final Object[] scenarioListValues, final Object[] regionListValues) throws Exception  {
		BaseTableModel bt = new ComboTableModel(qg, scenarioListValues, regionListValues, parentFrame, singleBinding, context);

		JTable jTable = bt.getAsSortedTable();
		new CopyPaste(jTable);

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
		JLabel labelChart = new JLabel();
		try {
			JFreeChart chart = bt.createChart(0,0);
			Dimension chartDim = bt.getChartDimensions(chart);
			BufferedImage chartImage = chart.createBufferedImage(
					(int)chartDim.getWidth(), (int)chartDim.getHeight());
			/*
			BufferedImage chartImage = chart.createBufferedImage(
					350, 350);
			 */


			labelChart.setIcon(new ImageIcon(chartImage));
		} catch(Exception e) {
			e.printStackTrace();
			labelChart.setText("Cannot Create Chart");
		}
		JSplitPane sp = new JSplitPane();
		sp.setLeftComponent(new JScrollPane(jTable));
		sp.setRightComponent(labelChart);
		sp.setDividerLocation(parentFrame.getWidth()-350-15);
		((InterfaceMain)parentFrame).fireProperty("Query", null, bt);
		return new JScrollPane(sp);
	}

}