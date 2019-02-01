package myPackage

import java.io.PrintWriter

import scala.math.BigInt.long2bigInt


class External_node(val mypoints: Array[Int]){

  var right: External_node = _
  var left: External_node = _
  var parent: External_node = _

  var nextDimensionRoot: External_node = _

  def printExternal_node(results_file : PrintWriter){
    print("(");
    results_file.write("\n");
    for ( i <- 0 to (mypoints.length - 1)) {
      results_file.write(mypoints(i).toString());
      print(mypoints(i));

      if(i!=(mypoints.length-1)){
        print(",");
        results_file.write(",");
      }
    }
    println(")");
  }

  def printExternal_node(){
    print("(");
    for ( i <- 0 to (mypoints.length - 1)) {
      print(mypoints(i));

      if(i!=(mypoints.length-1)){
        print(",");
      }
    }
    println(")");
  }

  def print_leaves(results_file : PrintWriter){
    go_to_child(this,0,results_file);
  }

  def print_leaves(){
    go_to_child(this,0);
  }

  def go_to_child(previous_call_node: External_node,side : Int,results_file : PrintWriter){
    var previous_node : External_node = previous_call_node
    if(side==0){
      previous_node = this;
    }
    else if(side==1){
      previous_node=previous_node.left;
    }
    else{
      previous_node=previous_node.right;
    }
    if(previous_node.left!=null) {
      go_to_child(previous_node,1,results_file);
    }
    if(previous_node.right!=null) {
      go_to_child(previous_node, 2,results_file);
    }

    if(previous_node.is_leaf()) {
      previous_node.printExternal_node(results_file)
    }

    previous_node = previous_node.parent;
  }

  def go_to_child(previous_call_node: External_node,side : Int){
    var previous_node : External_node = previous_call_node
    if(side==0){
      previous_node = this;
    }
    else if(side==1){
      previous_node=previous_node.left;
    }
    else{
      previous_node=previous_node.right;
    }
    if(previous_node.left!=null) {
      go_to_child(previous_node,1);
    }
    if(previous_node.right!=null) {
      go_to_child(previous_node, 2);
    }

    if(previous_node.is_leaf()) {
      previous_node.printExternal_node()
    }

    previous_node = previous_node.parent;
  }

  def is_leaf(): Boolean ={
   if(this.right==null && this.left==null) return true;
   else return false;
  }

}