package ModelInterface.ModelGUI2.xmldb;

/**
 * An interface for binding a query with the selected scenarios and
 * regions.
 * @author Pralit Patel
 */ 
public interface QueryBinding {
	/**
	 * Binds a query to the scenarios and regions passed in.  This 
	 * should return a complete query which is executable.
	 * @param scenarios List of selected scenarios.
	 * @param regions List of selected regions.
	 * @return A complete XQuery expression.
	 */
	public String bindToQuery(Object[] scenarios, Object[] regions);
}
