/**
  * Created by alessandrotorcetta on 27/02/17.
  */


import java.io.FileNotFoundException

import scala.io.Source


import scala.collection.mutable.ArrayBuffer   //per creare dynamic Array


object Test  extends  TorcettaParser{



  def main(args: Array[String]) = {

    //lettura file
    val filename = "src/testo.txt"

    var content = ArrayBuffer[String]()

    var myString: String  = ""



    try {
      for (line <- Source.fromFile(filename).getLines()) {
        // per tutte le lines del file
        println("Line dentro il for --> " + line)
        content += line
        myString = myString + line + "\n"

      }

      //PARSING COMBINATOR
      println("Eseguo il parsing totale... ")
      println(parseAll(axiom,myString))



      println("LINEs TOT:" + "\n" + myString)

      var i = 0
      for(x <- content) {
        i = i +1

        for (carattere <- x){
          //println("CHAR--> " + carattere)
          matchTermAndNonTerm(carattere.toString)
        }
        println("Line n° "+ i +" ---> " + x)
        matchProd(x.toString)

      }

      getContentMap()
      controlTest()

    }catch {
      case e: FileNotFoundException => println(e)
      case e: Exception => println(e)



    }

  }
}