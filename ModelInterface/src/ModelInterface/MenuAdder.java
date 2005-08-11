/**
 * 
 */
package ModelInterface;

import ModelInterface.InterfaceMain.MenuManager;

/**
 * Interface which defines that a class is capable of adding items to a set of
 * menus.
 * 
 * @author Josh Lurz
 * 
 */
public interface MenuAdder {
	/**
	 * Add menu items to the menu manager is specific locations.
	 * 
	 * @param aMenuManager
	 *            The menu manager to which to add menus and menu items.
	 */
	public void addMenuItems(MenuManager aMenuManager);
}
