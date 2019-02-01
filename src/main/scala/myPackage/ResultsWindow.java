/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package myPackage;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Array;

/**
 *
 * @author Johnara
 */
public class ResultsWindow extends javax.swing.JFrame {

    /**
     * Creates new form ResultsWindow
     */
    public ResultsWindow(String results_file,String ranges) {
        initComponents();
        load_file_to_table(results_file);
        this.setTitle(ranges);
    }

    private void load_file_to_table(String results_file) {
        try {
            FileReader fileReader = new FileReader(results_file);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String line;
            boolean first=true;
            while (((line = bufferedReader.readLine()) != null)) {
                int i=0;
                if(first){
                    String[] results  = line.split(",");
                    DefaultTableModel model = (DefaultTableModel) jTableResults.getModel();
                   while(i<results.length) {
                       model.addColumn(results[i]);
                       i++;
                   }
                    first=false;
                }
               else {
                    String[] results = line.split(",");
                    Integer[] results_int = new Integer[results.length];
                    for(int x=0; i<results.length;i++) {
                        results_int[i] = Integer.parseInt(results[i]); // So that sort in jtable works correct
                    }
                    DefaultTableModel model = (DefaultTableModel) jTableResults.getModel();
                    model.addRow(results_int);

                }
            }

            bufferedReader.close();
        } catch (FileNotFoundException ex) {
            System.out.println(
                    "Unable to open file '"
                            + results_file + "'");
        } catch (IOException ex) {
            System.out.println(
                    "Error reading file '"
                            + results_file + "'");
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTableResults = new javax.swing.JTable();

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        jTableResults.setAutoCreateRowSorter(true);
        jTableResults.setModel(new javax.swing.table.DefaultTableModel(
                new Object [][] {
                },
                new String [] {
                }
        ) {

            public Class getColumnClass(int columnIndex) {
                return Integer.class;
            }
        });
        jScrollPane1.setViewportView(jTableResults);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 400, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>

    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTableResults;
    // End of variables declaration
}
