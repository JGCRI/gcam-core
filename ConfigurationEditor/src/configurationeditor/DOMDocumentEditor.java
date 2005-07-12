/**
 * 
 */
package configurationeditor;

import org.w3c.dom.Document;

/**
 * The editor interface specifies that the implementing class is an editor
 * of a DOM document.
 * @author Josh Lurz
 *
 */
public interface DOMDocumentEditor {
    /**
     * Get the editor's document.
     * @return The editor's document.
     */
    public Document getDocument();
    
    /**
     * Returns whether to ask the user before saving or automatically saving.
     * @return Whether to ask the user before saving.
     */
    public boolean askBeforeSaving();
}
