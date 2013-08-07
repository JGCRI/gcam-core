package ModelInterface.ModelGUI2;

/**
 * Simple immutabe class that holds a scenario name, date, and docName for a
 * sceanario in a XML database.  These are used as the values in the 
 * scenario list in the DbViewer.  They have been sepearted from DbViewer
 * becuase they are now required to run a query.
 * @author Pralit Patel
 */
public class ScenarioListItem {
	/**
	 * The name of the internal xmldb document that contains this
	 * scenario.
	 */
	private final String docName;

	/**
	 * The name of the scenario.
	 */
	private final String scnName;
	/**
	 * The date of the scenario.
	 */
	private final String scnDate;

	/**
	 * Constructor to get the values of our fields.
	 */
	public ScenarioListItem(String docName, String scnName, String scnDate) {
		this.docName = docName;
		this.scnName = scnName;
		this.scnDate = scnDate;
	}

	/**
	 * Returns only the name+' '+date so that it displays with that info
	 * in a JList.  The docName should be be displayed as it is only necessary
	 * for internal use.
	 * @return A String suitable for display.
	 */ 
	public String toString() {
		// do not display docName to avoid clutter
		final String displayStr = scnName+' '+scnDate;
		return displayStr;
	}

	/**
	 * Get the internal doc name.
	 * @return The internal doc name.
	 */ 
	public String getDocName() {
		return docName;
	}

	/**
	 * Get the scenario name.
	 * @return The scneario name. 
	 */
	public String getScnName() {
		return scnName;
	}

	/**
	 * Get the scenario date.
	 * @return The scenario date.
	 */
	public String getScnDate() {
		return scnDate;
	}
}
