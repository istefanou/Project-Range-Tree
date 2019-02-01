package myPackage

import java.util

import scala.io.Source

class FileParser() {

  var debug=0;
  var filename:String=_;
  var delimiter:String=_;
  var dimensions:Int=_;
  val column_names:util.HashMap[Int, String]=new util.HashMap[Int,String];

  def set_column_names(){
    val iterator = Source.fromFile(filename).getLines();
    val line = iterator.next();
    val result = line.split(delimiter)
    var d = 0;
    for (item <- result) {
      column_names.put(d,item);
      d = d + 1;
    }
    this.dimensions=d;
  }

  def lines(): Int = {
    val lines = io.Source.fromFile(filename).getLines.size

    if(debug==1) println("Lines of file : " + lines);

    return lines;
  }

  def items(): Array[Array[Int]] = {
    val input_items: Array[Array[Int]] = Array.ofDim[Int](this.lines() - 1, this.dimensions)
    //lines-1 because first line contains column name
    var i = 0;
    for (line <- Source.fromFile(filename).getLines.drop(1)) {
      if (debug == 1) println("line : " + line);


      val result = line.split(delimiter)
      var d = 0;
      for (item <- result) {
        input_items(i)(d) = item.toInt;
        d = d + 1;
      }
      i = i + 1;
    }
    return input_items;
  }

  def set_filename(temp_filename : String): Unit ={
    this.filename=temp_filename;
    this.set_column_names();
  }

  def set_delimiter(temp_delimiter : String): Unit ={
    this.delimiter=temp_delimiter;
  }

}
