package ModelInterface.ModelGUI2.xmldb;

/**
 * Wraps a QueryBinding and simply appends an import statement
 * which will point to the ModelInterface XQuery function 
 * library which is in the mi namespace.
 * @author Pralit Patel
 */
public class ImportDecoratorQueryBinding implements QueryBinding {
	private QueryBinding decoratedBinding;
	public ImportDecoratorQueryBinding(QueryBinding decoratedBinding) {
		this.decoratedBinding = decoratedBinding;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		return "import module namespace mi = \"https://128.8.246.24/svn/ModelInterface\" at \"file:/mi.xquery\";\n"+
			decoratedBinding.bindToQuery(scenarios, regions);
	}
}
