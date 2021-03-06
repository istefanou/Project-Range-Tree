package myPackage;/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import javax.swing.*;
import java.awt.Dimension;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;

/**
 *
 * @author Johnara
 */
public class JFrameCompleteQuery extends javax.swing.JFrame {
        int size;
        HashMap dimensions = new HashMap();
        myPackage.Range_tree this_range_tree ;
        HashMap dimension_names = new HashMap();
        RangeTreeGUI parent_gui;

    JFrameCompleteQuery(Range_tree range_tree, HashMap<Object, String> temp_dimension_names, RangeTreeGUI main_menu_gui) {
        size=temp_dimension_names.size();
        initComponents();
        this_range_tree=range_tree;
        parent_gui = main_menu_gui;
        for (int x = 0; x <= size-1; x++) {
        JPanelQuery pq = new JPanelQuery(temp_dimension_names.get(x));
        dimension_names.put(x,temp_dimension_names.get(x));
        pq.setVisible(true);
        dimensions.put(x,pq);
            jPanel1.add(pq, x);
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents

    public void destroy() {
        this.this_range_tree=null;
        this.removeAll();
        this.dispose();
    }

    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setBounds(new java.awt.Rectangle(0, 0, 0, 0));
        setPreferredSize(new Dimension(423,(69*(size+1))));
        setResizable(false);
        setSize(new java.awt.Dimension(0, 0));
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosed(java.awt.event.WindowEvent evt) {
                formWindowClosed(evt);
            }
        });

        jPanel1.setLayout(new java.awt.GridLayout(size+1, 1));

        jMenu1.setText("Execute");
        jMenu1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jMenuExecuteMouseClicked(evt);
            }
        });
        jMenuBar1.add(jMenu1);

        setJMenuBar(jMenuBar1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(0, 211, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(267, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void formWindowClosed(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosed

    }//GEN-LAST:event_formWindowClosed

    private void jMenuExecuteMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jMenu1MouseClicked
        System.err.println("EXECUTE");

        try {
        HashMap ranges = new HashMap();
       int i=0;
       String range_name="";
        for (int x = 0; x <= size-1; x++) {
            JPanelQuery temp = (JPanelQuery) dimensions.get(x);

                if (temp.jTextFieldLeft.getText().length()>0 && temp.jTextFieldRight.getText().length()>0) {
                    if(Integer.parseInt(temp.jTextFieldLeft.getText())<=Integer.parseInt(temp.jTextFieldRight.getText())) {
                        range_name = range_name + "(" + dimension_names.get(x) + "," + temp.jTextFieldLeft.getText() + "-" + temp.jTextFieldRight.getText() + ")";
                        ranges.put(i++, x);
                        System.out.println(ranges.get(i - 1));
                        ranges.put(i++, Integer.parseInt(temp.jTextFieldLeft.getText()));
                        System.out.println(ranges.get(i - 1));
                        ranges.put(i++, Integer.parseInt(temp.jTextFieldRight.getText()));
                        System.out.println(ranges.get(i - 1));
                    }else{
                        JOptionPane.showMessageDialog(null,temp.jTextFieldLeft.getText()+" bigger than " + temp.jTextFieldRight.getText() + " !");
                    }
                }
                temp.jTextFieldLeft.setText("");
                temp.jTextFieldRight.setText("");
        }
            String fileName = range_name+new SimpleDateFormat("_yyyy_MM_dd_HH_mm'.txt'").format(new Date());
            File file = new File(fileName);
            file.createNewFile();
            if(parent_gui.diagram!=null)
            {
                parent_gui.jPanelTools.setVisible(false);
                parent_gui.diagram.draw_results(ranges);
                parent_gui.jLabelVertical.setVisible(false);
                parent_gui.jLabelVertical.setVisible(true);
                parent_gui.jLabelHorizontal.setVisible(false);
                parent_gui.jLabelHorizontal.setVisible(true);
            }
            this_range_tree.parse_query(ranges,fileName);
            ResultsWindow result_window = new ResultsWindow(fileName,range_name);
            result_window.setVisible(true);
        }catch(Exception e){
            System.out.println("Please insert proper values!");
        }

          for (int x = 0; x <= size-1; x++) {
                    JPanelQuery temp = (JPanelQuery) dimensions.get(x);
                    temp.jTextFieldLeft.setText("");
                    temp.jTextFieldRight.setText("");
                }

    }//GEN-LAST:event_jMenu1MouseClicked

    /**
     * @param args the command line arguments
     */


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables
}
