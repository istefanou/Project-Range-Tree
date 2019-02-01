package myPackage

class Tree(items: Array[Array[Int]] ,extra_dimensions: Int,current_dimension: Int) {

  var root: External_node = _
  var debug=0;
  var previous_base_node: External_node = _
  var node_sorting_helper = new Array[Int](math.ceil(math.log10(items.size)/math.log10(2)).toInt+1);//number of values from A // needs to have log<base2> of items.size
  var node_sorting_helper_counter: Int=0;
  var next_dimension_tree_node_contents:Array[Array[Int]]=Array.ofDim[Int](items.size,extra_dimensions+1)
  //var node_contents = new Array[Array[Array[Int]]](dimensions-1)(items.size);//ARRAYS FOR EVERY DIMENSION WITH ARRAYS FOR THE NEW DIMENSION TREE WITH ARRAYS OF THE VALUES
  var node_content_counter: Int=0;
  var trees_of_next_dimension_built: Int=0;
 // sort(items,0,items.size-1,current_dimension);

  build_basic_tree(0,items.size-1,0 );
  def build_basic_tree(left : Int , right : Int ,side : Int){
    var middle = (left+right)/2;


    pre_processing_main_tree(left, right,side, middle);

    if(current_dimension<extra_dimensions) build_next_dimension_subtree(side); // Pre-process and sort the multidimensional subtrees

    previous_base_node = previous_base_node.parent;
    if(debug==1) println("backtracking");

  }

  def pre_processing_main_tree(left : Int , right : Int ,side : Int, middle : Int): Unit={

    creation_of_main_tree(side, middle)

    if(left!=right) {
      if(debug==1) println("Move left");
      build_basic_tree(left, middle,1);
      if(debug==1) println("Move right");
      build_basic_tree(middle + 1, right,2);
    }
  }

  def creation_of_main_tree(side: Int, middle: Int):Unit={
    side match{
      case 0 => build_root(middle)
      case 1 => build_left_external(middle)
      case 2 => build_right_external(middle)
    }
  }

  def build_root(middle: Int): Unit={
    root = new External_node(items(middle))
    previous_base_node = root;
  }

  def build_left_external(middle: Int): Unit={
    previous_base_node.left = new External_node(items(middle))
    previous_base_node.left.parent = previous_base_node;
    previous_base_node=previous_base_node.left;
  }

  def build_right_external(middle: Int): Unit={
    previous_base_node.right = new External_node(items(middle))
    previous_base_node.right.parent = previous_base_node;
    previous_base_node=previous_base_node.right;
  }

  def build_next_dimension_subtree(side: Int):Unit = {
    if(previous_base_node.is_leaf()) {
        next_dimension_tree_node_contents(node_content_counter)=previous_base_node.mypoints;
      node_content_counter=node_content_counter+1;
      node_sorting_helper(node_sorting_helper_counter)=1;
      node_sorting_helper_counter=node_sorting_helper_counter+1;
    }
    if (side == 2) {
      val amount_of_numbers = node_sorting_helper(node_sorting_helper_counter - 2) + node_sorting_helper(node_sorting_helper_counter - 1)
      sort(next_dimension_tree_node_contents, node_content_counter - amount_of_numbers, node_content_counter - 1, current_dimension+1);
      {
        trees_of_next_dimension_built = trees_of_next_dimension_built +1;
        if(current_dimension==0) {
          println((trees_of_next_dimension_built*100)/(items.size-1)+"%")
        }
        previous_base_node.parent.nextDimensionRoot = new Tree(copy_array(next_dimension_tree_node_contents, node_content_counter - amount_of_numbers, node_content_counter - 1), extra_dimensions, current_dimension + 1).root
      }

      node_sorting_helper(node_sorting_helper_counter - 2) = amount_of_numbers;
      node_sorting_helper_counter = node_sorting_helper_counter - 1;

    }

  }

  def copy_array(original_array :Array[Array[Int]] ,left : Int,right : Int ):Array[Array[Int]]={
    val temp_array: Array[Array[Int]] = Array.ofDim[Int]((right-left)+1, extra_dimensions+1)
    var j :Int=0;
    for ( i <- left to right) {
      temp_array(j)=original_array(i)
    j=j+1;
    }
    return temp_array;
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
