package myPackage;

import scala.collection.mutable.HashMap;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

@SuppressWarnings("serial")
public class DiagramGraph extends JPanel {
    private static final int PREF_W = 200;
    private static final int PREF_H = 200;
    private static final int BORDER_GAP = 30;
    private static final Color GRAPH_COLOR = Color.green;
    private static final Color GRAPH_POINT_COLOR = new Color(150, 50, 50, 180);
    private static final Stroke GRAPH_STROKE = new BasicStroke(3f);
    private static final int GRAPH_POINT_WIDTH = 3;
    private static final int Y_HATCH_CNT = 10;
    private int[][] items;
    private int horizontal_dimension,vertical_dimension;
    private int horizontal_max =0, vertical_max =0;
    private int horizontal_left,horizontal_right,vertical_left,vertical_right;
    private boolean vertical_query_set=false,horizontal_query_set=false;
    public DiagramGraph(int[][] items,int horizontal,int vertical) {
        this.items = items;
        this.horizontal_dimension = horizontal;
        this.vertical_dimension = vertical;
        find_maxes();
    }

    @Override
    protected void paintComponent(Graphics g) {
        System.out.println("CALLED PAINT COMPONENTS");
        if (items != null) {
            super.paintComponent(g);
            Graphics2D g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            double xScale = ((double) getWidth() - 2 * BORDER_GAP) / horizontal_max;
            double yScale = ((double) getHeight() - 2 * BORDER_GAP) / vertical_max;
                System.out.println("xScale : " + xScale + " yScale " + yScale);
                List<Point> graphPoints = new ArrayList<Point>();
                for (int i = 0; i < items.length; i++) {
                    int x1 = (int) ((items[i][horizontal_dimension] * xScale) + BORDER_GAP);
                    int y1 = (int) (getHeight() - BORDER_GAP - (items[i][vertical_dimension] * yScale));
                    Point temp_point = new Point(x1, y1);
                    graphPoints.add(temp_point);
                }
                // create x and y axes
                g2.drawLine(BORDER_GAP, getHeight() - BORDER_GAP, BORDER_GAP, BORDER_GAP);
                g2.drawLine(BORDER_GAP, getHeight() - BORDER_GAP, getWidth() - BORDER_GAP, getHeight() - BORDER_GAP);

                // create hatch marks for y axis.
                for (int i = 0; i < Y_HATCH_CNT; i++) {
                    int x0 = BORDER_GAP;
                    int x1 = GRAPH_POINT_WIDTH + BORDER_GAP;
                    int y0 = getHeight() - (((i + 1) * (getHeight() - BORDER_GAP * 2)) / Y_HATCH_CNT + BORDER_GAP);
                    int y1 = y0;
                    g2.drawLine(x0, y0, x1, y1);
                    g.drawString(String.valueOf((vertical_max * (i + 1) / (Y_HATCH_CNT))), x0 - BORDER_GAP / 2, y1);
                }

                // and for x axis
                for (int i = 0; i < (items.length - 1); i++) {
                    int x0 = (i + 1) * (getWidth() - BORDER_GAP * 2) / (items.length - 1) + BORDER_GAP;
                    int x1 = x0;
                    int y0 = getHeight() - BORDER_GAP;
                    int y1 = y0 - GRAPH_POINT_WIDTH;
                    g2.drawLine(x0, y0, x1, y1);
                    g.drawString(String.valueOf((horizontal_max * (i + 1) / (items.length - 1))), x1, y0 + BORDER_GAP / 2);

                }

                Stroke oldStroke = g2.getStroke();
                g2.setColor(GRAPH_COLOR);
                g2.setStroke(GRAPH_STROKE);
                for (int i = 0; i < graphPoints.size() - 1; i++) {
                    int x1 = graphPoints.get(i).x;
                    int y1 = graphPoints.get(i).y;
                    int x2 = graphPoints.get(i + 1).x;
                    int y2 = graphPoints.get(i + 1).y;
                    // g2.drawLine(x1, y1, x2, y2);
                }

                g2.setStroke(oldStroke);
                g2.setColor(GRAPH_POINT_COLOR);
                for (int i = 0; i < graphPoints.size(); i++) {
                    int x = graphPoints.get(i).x - GRAPH_POINT_WIDTH / 2;
                    int y = graphPoints.get(i).y - GRAPH_POINT_WIDTH / 2;
                    ;
                    int ovalW = GRAPH_POINT_WIDTH;
                    int ovalH = GRAPH_POINT_WIDTH;
                    g2.fillOval(x, y, ovalW, ovalH);


                }

            if(horizontal_query_set || vertical_query_set) {
                g2.setColor(GRAPH_COLOR);
                g2.setStroke(g2.getStroke());
                g2.drawLine((int) ((horizontal_left* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_left * yScale)), (int) ((horizontal_left* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_right * yScale)));
                g2.drawLine((int) ((horizontal_right* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_left * yScale)), (int) ((horizontal_right* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_right * yScale)));


                g2.drawLine((int) ((horizontal_left* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_left * yScale)), (int) ((horizontal_right* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_left * yScale)));

                g2.drawLine((int) ((horizontal_left* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_right * yScale)), (int) ((horizontal_right* xScale) + BORDER_GAP), (int) (getHeight() - BORDER_GAP - (vertical_right * yScale)));
                vertical_query_set=false;
                horizontal_query_set=false;
                System.out.println("REPAINT");
            }

        }
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(PREF_W, PREF_H);
    }

    private void find_maxes(){
        for(int i=0; i<items.length; i++) {
            if(items[i][horizontal_dimension]>horizontal_max) horizontal_max=items[i][horizontal_dimension];
            if(items[i][vertical_dimension]>vertical_max) vertical_max=items[i][vertical_dimension];
        }
    }


    public void draw_results(java.util.HashMap ranges) {
        vertical_left=0;
        vertical_right=vertical_max;
        horizontal_left=0;
        horizontal_right=horizontal_max;
        for (int x = 0; x < ranges.size()-1; x=x+3) {
            if((int)ranges.get(x)==vertical_dimension){
                vertical_left=(int) ranges.get(x+1);
                vertical_right=(int) ranges.get(x+2);
                vertical_query_set=true;
            }
            if((int)ranges.get(x)==horizontal_dimension){
                horizontal_left=(int) ranges.get(x+1);
                horizontal_right=(int) ranges.get(x+2);
                horizontal_query_set=true;
            }
    }
    repaint();

    }
}