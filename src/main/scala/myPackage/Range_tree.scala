package myPackage

import java.util
import java.io._

import javafx.scene.control.ProgressBar

import scala.collection.mutable


class Range_tree(val items: Array[Array[Int]] ,extra_dimensions: Int,column_names: util.HashMap[Int,String]) {



  sort(items,0,items.size-1,0);
  var base = new Tree(items,extra_dimensions,0).root;
  var debug=0;

  def parse_query(query_ranges: util.HashMap[Int,Int],filename: String): Unit = {
    val myQueue: mutable.Queue[Int] = new mutable.Queue[Int]()
    for (i <- 0 to (query_ranges.size()-1)){
      myQueue.enqueue(query_ranges.get(i))
    }
    query(myQueue,filename);
  }

  def query(query_ranges: mutable.Queue[Int],filename: String) {

    var previous_marked_nodes:mutable.Queue[External_node]=scala.collection.mutable.Queue[External_node]()
    var marked_nodes:mutable.Queue[External_node]=scala.collection.mutable.Queue[External_node]()
    marked_nodes.enqueue(base);
    var current_dimension_of_previous = 0;
    do{
      var query_dimension = query_ranges.dequeue()
      var query_left = query_ranges.dequeue()
      var query_right = query_ranges.dequeue()
      while(!marked_nodes.isEmpty) {previous_marked_nodes.enqueue(marked_nodes.dequeue());}
      while(!previous_marked_nodes.isEmpty) {//query for spesific dimension
       var temp_node:External_node= previous_marked_nodes.dequeue();
        if (temp_node.nextDimensionRoot != null ) {
        marked_nodes = marked_nodes ++ get(find_correct_dimension_node_query(temp_node, current_dimension_of_previous, query_dimension), query_dimension, query_left, query_right)
      }
        else if(temp_node.mypoints(query_dimension)>=query_left && temp_node.mypoints(query_dimension)<=query_right){
          marked_nodes.enqueue(temp_node)
        }
      }
      current_dimension_of_previous=query_dimension;

    }while(!query_ranges.isEmpty)

    var i: Int=0;
    val results_file = new PrintWriter(filename)
    println("Results : ")

    for ( i <- 0 to (column_names.size - 1)) {
      results_file.write(column_names.get(i))
      if(i!=column_names.size - 1) results_file.write(",")
    }

    while(!marked_nodes.isEmpty){// for every node with possible results
    var temp_node=marked_nodes.dequeue()
      temp_node.print_leaves(results_file);
    }
    println();
    results_file.close();
  }

  def get(root_node: External_node, current_dimension: Int, left: Int, right: Int ): mutable.Queue[External_node]={
    //root node is root of the tree that might contains answers of the query
    var marked_nodes = scala.collection.mutable.Queue[External_node]()
    var current_node_left = root_node
    var current_node_right = root_node
    var split_external_node: External_node = null
    var left_reached_leaf=false;
    var right_reached_leaf=false;
    var left_streak: Int=0;
    var right_streak: Int=0;
    var left_went_right=true;
    var right_went_right=true;


    var left_child_of_split_node_found=false;
    var right_child_of_split_node_found=false;
    println("--------Limiting data of node : "+root_node.mypoints(current_dimension) +" with L :"+left+" R : "+right +" on dimension " + current_dimension +"----------");

    //to make function handle leaves
    if(root_node.left==null) left_reached_leaf=true;
    if(root_node.right==null) right_reached_leaf=true;

    while (!left_reached_leaf || !right_reached_leaf) {


      if (!left_reached_leaf) {
        if (left > current_node_left.mypoints(current_dimension)) {
          current_node_left = current_node_left.right
          left_went_right = true;
        }
        else if (left <= current_node_left.mypoints(current_dimension)) {
          current_node_left = current_node_left.left
          left_went_right = false;
        }
      }

      if (!right_reached_leaf) {

        if (right <= current_node_right.mypoints(current_dimension)) {
          current_node_right = current_node_right.left
          right_went_right = false;
        } else if (right > current_node_right.mypoints(current_dimension)) {
          current_node_right = current_node_right.right
          right_went_right = true;
        }
      }

      if (right_went_right!=left_went_right && split_external_node==null) {
        split_external_node=current_node_left.parent
      }


      if (!left_reached_leaf) {
        //LEFT QUERY RANGE

        if(left_went_right){// GOING RIGHT
          if(debug==2) println("Left node went right");
          if(!current_node_left.is_leaf()){// IS NOT LEAF
            left_streak=0;
          }
          else{//IS LEAF
            if (current_node_left.is_leaf) left_reached_leaf = true
            if (left <= current_node_left.mypoints(current_dimension)) {//acceptable value from left range
              if (current_node_left.mypoints(current_dimension) <= right && !marked_nodes.contains(current_node_left)) {
                println("Check leaf : " + current_node_left.mypoints(current_dimension))
                marked_nodes.enqueue(current_node_left)
              }
            }
            while(current_node_left.parent!=split_external_node && split_external_node!=null){//go back up to split node - 1
              if(current_node_left.parent.left==current_node_left){// if comming up from left child
                //mark current_node_left.parent.right
                println("Mark node : "+current_node_left.parent.right.mypoints(current_dimension))
                current_node_left.parent.right.print_leaves()
                marked_nodes.enqueue(current_node_left.parent.right)
              };
              current_node_left=current_node_left.parent;
            }
          }
        }

        if(!left_went_right){// GOING LEFT
          if(debug==2) println("Left node went left");
          left_streak=left_streak+1;
          if(current_node_left.is_leaf()){// IS LEAF
            if (current_node_left.is_leaf) left_reached_leaf = true
            if (left <= current_node_left.mypoints(current_dimension)){//acceptable value
              while(current_node_left.parent!=split_external_node && split_external_node!=null && left_streak>0 ){//go back left streak nodes up to split node - 1

                current_node_left=current_node_left.parent;
                left_streak=left_streak-1;
              }
              //mark current_node_left
              if(split_external_node!=null) {
                println("Mark node : " + current_node_left.mypoints(current_dimension))
                if (current_node_left == split_external_node.left) {
                  left_child_of_split_node_found = true;
                }
                else {
                  current_node_left.print_leaves()
                  marked_nodes.enqueue(current_node_left)
                }
              };
              else {
                if(current_node_left.mypoints(current_dimension)<=right && !marked_nodes.contains(current_node_left)) {
                  println("Check leaf : " + current_node_left.mypoints(current_dimension))
                  marked_nodes.enqueue(current_node_left)
                }
              }
            }

            while(current_node_left.parent!=split_external_node && split_external_node!=null){//go back up to split node - 1
              if(current_node_left.parent.left==current_node_left){// if comming up from right child
                //mark current_node_left.parent.right
                println("Mark node : "+current_node_left.parent.right.mypoints(current_dimension));
                current_node_left.parent.right.print_leaves()
                marked_nodes.enqueue(current_node_left.parent.right)
              };
              current_node_left=current_node_left.parent;
            }
          }
        }
      }


      if (!right_reached_leaf) {//RIGHT QUERY RANGE

        if(!right_went_right){// GOING LEFT

          if(debug==2) println("Right node went left");
          if(!current_node_right.is_leaf()){// IS NOT LEAF
            right_streak=0;
          }
          else{//IS LEAF
            if (current_node_right.is_leaf) right_reached_leaf = true
            if (right >= current_node_right.mypoints(current_dimension)) {
              //acceptable value
              //check leaf for the rest of the query
              if (current_node_right.mypoints(current_dimension) >= left && !marked_nodes.contains(current_node_right)) {
                println("Check leaf : " + current_node_right.mypoints(current_dimension))
                marked_nodes.enqueue(current_node_right)
              }
            }
            while(current_node_right.parent!=split_external_node && split_external_node!=null){//go back up to split node - 1
              if(current_node_right.parent.right==current_node_right){// if comming up from right child
                //mark current_node_right.parent.left
                println("Mark node : "+current_node_right.parent.left.mypoints(current_dimension))
                current_node_right.parent.left.print_leaves()
                marked_nodes.enqueue(current_node_right.parent.left)
              };
              current_node_right=current_node_right.parent;
            }
          }
        }

        if(right_went_right){//GOING RIGHT
          if(debug==2)  println("Right node went right");
          right_streak=right_streak+1;
          if(current_node_right.is_leaf()){// IS LEAF
            if (current_node_right.is_leaf) right_reached_leaf = true
            if (right >= current_node_right.mypoints(current_dimension)){//acceptable value
              while(current_node_right.parent!=split_external_node && split_external_node!=null && right_streak>0 ){//go back right streak nodes up to split node - 1

                current_node_right=current_node_right.parent;
                right_streak=right_streak-1;
              }
              //mark current_node_right
              if(split_external_node!=null){
                println("Mark node : " + current_node_right.mypoints(current_dimension))
                if(current_node_right==split_external_node.right){
                  right_child_of_split_node_found=true;
                }
                else{
                  current_node_right.print_leaves()
                  marked_nodes.enqueue(current_node_right)
                }

              };
              else {
                if (current_node_right.mypoints(current_dimension) >= left && !marked_nodes.contains(current_node_right)) {
                  println("Check leaf : " + current_node_right.mypoints(current_dimension))
                  marked_nodes.enqueue(current_node_right)
                }
              }
            }

            while(current_node_right.parent!=split_external_node && split_external_node!=null){//go back up to split node - 1
              if(current_node_right.parent.right==current_node_right){// if comming up from right child
                //mark current_node_right.parent.left

                println("Mark node : "+current_node_right.parent.left.mypoints(current_dimension));
                current_node_right.parent.left.print_leaves()
                marked_nodes.enqueue(current_node_right.parent.left)
              }
              current_node_right=current_node_right.parent;
            }
          }
        }
      }

    }
    if(debug==2) println("LEFT node : "+current_node_left.mypoints(current_dimension)+ " Right node : "+current_node_right.mypoints(current_dimension));
    if(debug==2 && split_external_node!=null) println(" Split node : "+split_external_node.mypoints(current_dimension));

    if(left_child_of_split_node_found && right_child_of_split_node_found) {

      split_external_node.print_leaves()
      marked_nodes.enqueue(split_external_node)
      println("Instead of node : "+split_external_node.left.mypoints(current_dimension)+ " and "+split_external_node.right.mypoints(current_dimension)+" marking : "+split_external_node.mypoints(current_dimension));
    }
    else if(left_child_of_split_node_found && !right_child_of_split_node_found){
      split_external_node.left.print_leaves()
      marked_nodes.enqueue(split_external_node.left)
    }
    else if(right_child_of_split_node_found && !left_child_of_split_node_found){
      split_external_node.right.print_leaves()
      marked_nodes.enqueue(split_external_node.right)
    }
    // else marked_nodes(marked_nodes_pointer)=null;
    println("--------End of limiting data with "+current_dimension+" dimension----------");
    return marked_nodes;

  }

  def find_correct_dimension_node_query( external_node: External_node,current_dimension: Int,final_dimension: Int): External_node = {//moves from current dimension to the correct one for the next query
    var current_node = external_node;
      for (i <- 1 to final_dimension - current_dimension) {
        current_node = current_node.nextDimensionRoot;
      }
    return current_node;
  }

  def sort(xs: Array[Array[Int]],left: Int,right: Int,dimension: Int): Unit = {
    def swap(i: Int, j: Int): Unit = {
      val t = xs(i); xs(i) = xs(j); xs(j) = t;
    }
    def sort1(l: Int, r: Int){
      val pivot = xs((l + r) / 2)(dimension);
      var i = l;
      var j = r;
      while (i <= j) {

        while (xs(i)(dimension) < pivot) { i = i + 1 }
        while (xs(j)(dimension) > pivot) {j = j - 1 }
        if (i <= j) {
          swap(i, j);
          i = i + 1;
          j = j - 1;
        }
      }
      if (l < j) sort1(l, j);
      if (j < r) sort1(i, r);
    }
    sort1(left, right);
    if(debug==1) println("END OF SORT");
  }
}
