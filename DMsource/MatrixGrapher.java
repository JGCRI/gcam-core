/*
 * This software, which is provided in confidence, was prepared by employees
 of Pacific Northwest National Laboratory operated by Battelle Memorial
 Institute. Battelle has certain unperfected rights in the software 
 which should not be copied or otherwise disseminated outside your 
 organization without the express written authorization from Battelle. All rights in
 the software are reserved by Battelle.  Battelle makes no warranty,
 express or implied, and assumes no liability or responsibility for the
 use of this software.
 */
/*!
 * \file *file*
 * \ingroup *project*
 * \brief *brief description*
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
package DMsource;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.image.*;
import javax.swing.*;

public class MatrixGrapher
{
  private static final int[] colorList = { 0x808080, 0x00006B, 0x0000CD, 0x0A32FF,
      0x2495EF, 0x24BFFF, 0x65DDFD, 0xAFEFF8, 0xFFFF4F, 0xFFE228, 0xFFC500, 0xFF6500,
      0xF40000, 0xBA0000, 0x650000 };
  final static String LOOKANDFEEL = "System";
  static int openWindows = 0;
 
  private double[][] toDisplay;
  private double minVal;
  private double maxVal;
  private double xPos;
  private double yPos;
  private double res;
  private double toWeight;
  private int xLength;
  private int yLength;

  public void drawMatrix(double[][] m, double min, double max, double x, double y, double r)
  {
    toDisplay = m;
    minVal = min;
    maxVal = max;
    xPos = x;
    yPos = y;
    res = r;
    toWeight = maxVal-minVal;
    xLength = toDisplay[0].length;
    yLength = toDisplay.length;

    //Set the look and feel.
    initLookAndFeel();

    //Make sure we have nice window decorations.
    JFrame.setDefaultLookAndFeelDecorated(true);

    //Create and set up the window.
    JFrame frame = new JFrame("Matrix Plot");
    frame.addWindowListener(new java.awt.event.WindowAdapter()
    {
      public void windowClosing(java.awt.event.WindowEvent e)
      {
        if(openWindows==1)
        {
          System.exit(0); // Terminate when the last window is closed.
        }
        openWindows--;
      }
    });
    //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    JPanel pane = new JPanel();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
    pane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
    pane.add(createImage());
    pane.add(createLabel());
    pane.add(createLegend());

    //Display the window.
    frame.setContentPane(pane);
    //frame.setResizable(false);
    frame.pack();
    if((int)(360/res) > 156)
    {
      frame.setSize(new Dimension((int)((360/res)+29), (int)((180/res)+86)));
    } else
    {
      frame.setSize(new Dimension(185, (int)((180/res)+86)));
    }
    
    openWindows++;
    frame.setVisible(true);
  }

  private Component createImage()
  {
    Canvas Mplot = makeImageFromMatrix();
    return Mplot;
  }
  
  private Component createLabel() 
  {
      JPanel pane = new JPanel();
      pane.setLayout(new BoxLayout(pane, BoxLayout.LINE_AXIS));
      JLabel label1 = new JLabel(minVal+"");
      JLabel label3 = new JLabel(maxVal+"");

      label1.setSize(new Dimension(100, 16));
      label3.setSize(new Dimension(100, 16));
      
      pane.add(label1);
      pane.add(Box.createRigidArea(new Dimension(110,5)));
      pane.add(label3);
      return pane;
  }
  
  private Component createLegend()
  {
    JPanel pane1 = new JPanel();
    pane1.setLayout(new BoxLayout(pane1, BoxLayout.LINE_AXIS));
    
    for(int i  = 0; i < 14; i++)
    {
        Canvas hold = new simpleBlockCanvas(colorList[i+1]);
        pane1.add(hold, null);
    }
    pane1.setMaximumSize(new Dimension(140,10));
    
    return pane1;
  } 

  private Canvas makeImageFromMatrix()
  {
    BufferedImage bi = new BufferedImage((int)(360/res), (int)(180/res), BufferedImage.TYPE_INT_RGB);
    double proportion;
    
    for(double iY = 90; iY > (-90); iY-=res)
    {
      if(((iY >= yPos+res))&&(iY <= (yPos+(yLength*res))))
      { //this whole line is inside the y bounds
        for(double iX = -180; iX < (180); iX+=res)
        {
          if(((iX >= xPos))&&(iX <= (xPos+(xLength*res)-res)))
          {
            if(Double.isNaN(toDisplay[(int)(((yPos+(yLength*res))-iY)/res)][(int)((iX-xPos)/res)]))
            {
              proportion = Double.NaN;
            } else
            {
              double holdThis = toDisplay[(int)(((yPos+(yLength*res))-iY)/res)][(int)((iX-xPos)/res)]-minVal;
              proportion = holdThis/toWeight;
            }
            
            if(Double.isNaN(proportion))
            {
              if((iX != -180)&&(iY != 90)&&(Math.IEEEremainder((iX), 30) == 0)||(Math.IEEEremainder((iY), 30) == 0))
              {
                bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), 0x000000);
              } else
              {
                bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[0]);
              }
            } else if(proportion==0)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[1]);
            } else if(proportion<=.077)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[2]);
            } else if(proportion<=.154)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[3]);
            } else if(proportion<=.231)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[4]);
            } else if(proportion<=.308)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[5]);
            } else if(proportion<=.385)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[6]);
            } else if(proportion<=.462)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[7]);
            } else if(proportion<=.539)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[8]);
            } else if(proportion<=.616)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[9]);
            } else if(proportion<=.693)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[10]);
            } else if(proportion<=.77)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[11]);
            } else if(proportion<=.847)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[12]);
            } else if(proportion<=.925)
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[13]);
            } else
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[14]);
            }
            
          } else
          {
            if((iX != -180)&&(iY != 90)&&(Math.IEEEremainder((iX), 30) == 0)||(Math.IEEEremainder((iY), 30) == 0))
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), 0x000000);
            } else
            {
              bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[0]);
            }
          }
        }
      } else
      {
        for(double iX = -180; iX < (180); iX+=res)
        {
          if((iX != -180)&&(iY != 90)&&(Math.IEEEremainder((iX), 30) == 0)||(Math.IEEEremainder((iY), 30) == 0))
          {
            bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), 0x000000);
          } else
          {
            bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[0]);
          }
        }
      }
    }

    Rectangle2D.Double anchor = new Rectangle2D.Double(0, 0, (360/res), (180/res));

    TexturePaint tp = new TexturePaint(bi, anchor);

    class matrixCanvas
        extends Canvas
    {
      TexturePaint tp;

      public matrixCanvas(TexturePaint t)
      {
        tp = t;
      }

      public void paint(Graphics g)
      {
        // step one of the recipe; cast Graphics object as Graphics2D
        Graphics2D g2d = (Graphics2D)g;

        // step two-set the graphics context
        g2d.setPaint(tp); //setting context

        //step three-render something
        g2d.fill(new Rectangle2D.Float(0, 0, (int)(360/res), (int)(180/res)));
      }
    }

    return new matrixCanvas(tp);
  }

  private static void initLookAndFeel()
  {
    String lookAndFeel = null;

    if(LOOKANDFEEL!=null)
    {
      if(LOOKANDFEEL.equals("Metal"))
      {
        lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
      } else
        if(LOOKANDFEEL.equals("System"))
        {
          lookAndFeel = UIManager.getSystemLookAndFeelClassName();
        } else
          if(LOOKANDFEEL.equals("Motif"))
          {
            lookAndFeel = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
          } else
            if(LOOKANDFEEL.equals("GTK+"))
            { //new in 1.4.2
              lookAndFeel = "com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
            } else
            {
              System.err.println("Unexpected value of LOOKANDFEEL specified: "
                  +LOOKANDFEEL);
              lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
            }

      try
      {
        UIManager.setLookAndFeel(lookAndFeel);
      } catch(ClassNotFoundException e)
      {
        System.err
            .println("Couldn't find class for specified look and feel:"+lookAndFeel);
        System.err.println("Did you include the L&F library in the class path?");
        System.err.println("Using the default look and feel.");
      } catch(UnsupportedLookAndFeelException e)
      {
        System.err.println("Can't use the specified look and feel ("+lookAndFeel
            +") on this platform.");
        System.err.println("Using the default look and feel.");
      } catch(Exception e)
      {
        System.err.println("Couldn't get specified look and feel ("+lookAndFeel
            +"), for some reason.");
        System.err.println("Using the default look and feel.");
        e.printStackTrace();
      }
    }
  }
  
  class simpleBlockCanvas extends Canvas 
  {
    int color;
    
    public simpleBlockCanvas(int c)
    {
        color = c;
        setSize(new Dimension(10,10));
    }
    
    public void paint(Graphics g) {

      // step one of the recipe; cast Graphics object as Graphics2D
      Graphics2D g2d = (Graphics2D) g;

      // step two-set the graphics context
      g2d.setColor(new Color(color)); //setting context

      //step three-render something
      g2d.fill(new Rectangle2D.Float(0,0,10,10));
    }
 }
}
