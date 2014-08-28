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
/*!
 * \file *file*
 * \ingroup *project*
 * \brief *brief description*
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date: 2006-06-16 15:44:26 -0400 (Fri, 16 Jun 2006) $
 * \version $Revision: 2752 $
 */
package ModelInterface.DMsource;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.awt.image.*;
import javax.swing.*;
import java.text.*;

public class MatrixGrapher implements ActionListener
{
  private static final int[] colorList = { 0x808080, 0x00006B, 0x0000CD, 0x0A32FF,
      0x2495EF, 0x24BFFF, 0x65DDFD, 0xAFEFF8, 0xFFFF4F, 0xFFE228, 0xFFC500, 0xFF6500,
      0xF40000, 0xBA0000, 0x650000 };
  final static String LOOKANDFEEL = "System";
  static int openWindows = 0;
  
  private JFormattedTextField zoomField;
  private JLabel imageLabel;
  private JLabel zoomDisp;
  private JScrollPane sp;
 
  private double[][] toDisplay;
  private double minVal;
  private double maxVal;
  private double xPos;
  private double yPos;
  private double res;
  private double toWeight;
  private int xLength;
  private int yLength;
  private int currZoom;

  public void drawMatrix(double[][] m, double min, double max, double x, double y, double r)
  {
    drawMatrix(new String("Generic Plot"), m, min, max, x, y, r);
  }
  
  public void drawMatrix(String name, double[][] m, double min, double max, double x, double y, double r)
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
    currZoom = 1;

    //Set the look and feel.
    initLookAndFeel();

    //Make sure we have nice window decorations.
    JFrame.setDefaultLookAndFeelDecorated(true);

    //Create and set up the window.
    JFrame frame = new JFrame(name);
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
    //frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    JPanel pane = new JPanel();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
    pane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
    pane.add(createScrollArea());
    pane.add(createLegendArea());
    pane.add(createZoomArea());

    //Display the window.
    frame.setContentPane(pane);
    //frame.setResizable(false);
    frame.pack();

    frame.setSize(new Dimension(650, 350));

    
    openWindows++;
    frame.setVisible(true);
  }

  private Component createScrollArea()
  {
    //Dimension dim = new Dimension(360, 180);
    sp = new JScrollPane(createImage(), JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    
    sp.setBackground(Color.white);
    //sp.setPreferredSize(dim);
    
    return sp;
  }
  
  private Component createImage()
  {
    ImageIcon ii = new ImageIcon(makeImageFromMatrix());
    imageLabel = new JLabel(ii);
    
    return imageLabel;
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

  private Component createLegendArea()
  {
    JPanel LegPane = new JPanel();
    LegPane.setLayout(new BoxLayout(LegPane, BoxLayout.LINE_AXIS));
    
    JLabel labelMin = new JLabel(minVal+"");
    JLabel labelMax = new JLabel(maxVal+"");
    labelMin.setHorizontalAlignment(SwingConstants.TRAILING);
    labelMax.setHorizontalAlignment(SwingConstants.LEADING);
    
    labelMin.setMinimumSize(new Dimension(120, 16));
    labelMin.setMaximumSize(new Dimension(120, 16));
    labelMax.setMinimumSize(new Dimension(120, 16));
    labelMax.setMaximumSize(new Dimension(120, 16));
    
    LegPane.add(labelMin);
    LegPane.add(Box.createRigidArea(new Dimension(5,18)));
    LegPane.add(createLegend());
    LegPane.add(Box.createRigidArea(new Dimension(5,18)));
    LegPane.add(labelMax);
    
    return LegPane;
  }
  
  private Component createZoomBox()
  {
    NumberFormat amountFormat = NumberFormat.getIntegerInstance();
    amountFormat.setMaximumIntegerDigits(3);
    zoomField = new JFormattedTextField(amountFormat);
    //zoomField.setValue(new Double(1));
    zoomField.setColumns(3);
    zoomField.setMaximumSize(new Dimension(40, 18));

    zoomField.addActionListener(this);
    
    return zoomField;
  }
  
  private Component createZoomButton()
  {
    JButton zoomButton = new JButton("Zoom");
    
    zoomButton.addActionListener(this);
    
    return zoomButton;
  }
  
  private Component createZoomDisplay()
  {
    zoomDisp = new JLabel("Current Zoom: "+currZoom+"x");
    //zoomDisp.setSize(new Dimension(25, 16));
    
    return zoomDisp;
  }
  
  private Component createZoomArea()
  {
    JPanel ZApane = new JPanel();
    ZApane.setLayout(new BoxLayout(ZApane, BoxLayout.LINE_AXIS));
    
    ZApane.add(createZoomButton());
    ZApane.add(Box.createRigidArea(new Dimension(20,5)));
    JLabel nZ = new JLabel("New Zoom: ");
    ZApane.add(nZ);
    ZApane.add(createZoomBox());
    ZApane.add(Box.createRigidArea(new Dimension(20,5)));
    ZApane.add(createZoomDisplay());
    
    ZApane.setPreferredSize(new Dimension(200, 20));
    
    return ZApane;
  }
  
  public void actionPerformed(ActionEvent e)
  {
    int zoom = Integer.valueOf(zoomField.getText());
    
    BufferedImage buff = makeImageFromMatrix();
    Image rescaled = buff.getScaledInstance((buff.getWidth()*zoom), (buff.getHeight()*zoom), Image.SCALE_FAST );
    
    ImageIcon newII = new ImageIcon(rescaled);
    imageLabel.setIcon(newII);
    
    currZoom = zoom;
    zoomDisp.setText("Current Zoom: "+currZoom+"x");
    
    imageLabel.revalidate();
    sp.revalidate();
  }
  
  private BufferedImage makeImageFromMatrix()
  {
    /*
     * if toWeight = 0 we have a problem, throw arithmetic exception and everything is colored grey
     * should this be a special case or dealt with procedurally?
     * Answer: just reset toWeight to 1 if it is 0, if toWeight is 0 there is only 1 value for
     * the entire matrix for this wont change output at all. of note: everything will be set
     * at the lowest color, though this isnt really a problem at all.
     */
    BufferedImage bi = new BufferedImage((int)(360/res), (int)(180/res), BufferedImage.TYPE_INT_RGB);
    double proportion;
    
    if(toWeight == 0)
    { //see above
      toWeight = 1;
    }
    
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
              if((iX != -180)&&(iY != 90)&&((iX%30) == 0)||((iY%30) == 0))
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
            if((iX != -180)&&(iY != 90)&&((iX%30) == 0)||((iY%30) == 0))
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
          if((iX != -180)&&(iY != 90)&&((iX%30) == 0)||((iY%30) == 0))
          {
            bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), 0x000000);
          } else
          {
            bi.setRGB((int)((iX+180)/res), (int)(Math.abs(iY-90)/res), colorList[0]);
          }
        }
      }
    }

    return bi;
  }

  private static void initLookAndFeel()
  {
    String lookAndFeel = null;

    if(LOOKANDFEEL!=null)
    {
      if(LOOKANDFEEL.equals("Metal"))
      {
        lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
      } else if(LOOKANDFEEL.equals("System"))
      {
        lookAndFeel = UIManager.getSystemLookAndFeelClassName();
      } else if(LOOKANDFEEL.equals("Motif"))
      {
        lookAndFeel = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
      } else if(LOOKANDFEEL.equals("GTK+"))
      { // new in 1.4.2
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
