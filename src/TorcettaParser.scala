/**
  * Created by alessandrotorcetta on 27/02/17.
  */

import java.util

import collection.mutable.{HashMap, ListBuffer}
import scala.util.parsing.combinator._

import scala.util.control.Breaks._



class TorcettaParser extends JavaTokenParsers {

  def axiom : Parser[Any] = I ~ B ~ C

  def I : Parser[Any] = "{" ~ term ~ rep("," ~ term) ~ "}"

  def B : Parser[Any] = "{" ~ nonTerm ~ rep("," ~ nonTerm) ~ "}"

  def C: Parser[Any] = Prod ~ opt(C)      //problema con i ritorni a capo

  def Prod: Parser[Any] = nonTerm ~ ":" ~ rep(term | nonTerm) ~ ("," | ".")
  def nonTerm: Parser[Any] = """[A-Z]""".r ^^ { _.toString }

  def term : Parser[Any] = """[a-z]""".r ^^ { _.toString } | '&' | '+' | '*' | '-' | '/'


  val nonTerminal = "([A-Z])".r
  val terminal = "([a-z]|[+]|[-]|[&]|[*]|[/])".r
  val production = "([A-Z][:][a-z-A-Z|[+]|[-]|[&]|[*]|[/]]*[,|.])".r


  var Mappa: HashMap[Char, List[String]] = new HashMap()        //HashMap in cui salvo per ogni NON TERMINALE la lista di produzioni in cui compare nella parte sx



  def matchTermAndNonTerm(x:Any): Any = x match {
    case nonTerminal(x) => println("NON TERMINALE individuato")

                           if(!Mappa.contains(x.toCharArray.head) ) {
                             println("Lista associata a :" + x.toCharArray.head + " VUOTA")
                             Mappa.put(x.toCharArray.head, List())
                           }
                           else  {
                             println("Lista associata a :" + x.toCharArray.head + " NON VUOTA")
                             Mappa.put(x.toCharArray.head,  Mappa(x.toCharArray.head))
                           }

    case terminal(x) => println("TERMINALE individuato")



    case _ => println("Warning: non è stato riconosciuto nè un TERMINALE, nè un NON TERMINALE")
  }


  def matchProd(x:Any): Any = x match {

    case production(x) => println("Produzione Individuata #### : " + x.toString)
                          val key = x.toCharArray.head
                          println("Valore della key a cui aggiungo la produzione ##### ----> " + key)

                          var Lista: List[String] = Mappa(key)
                          var ListaBuffer = Lista.to[ListBuffer]
                          var initDel = 0
                          def endDel: Int = {
                            if (x.toString.contains(",")) return x.toString.indexOf(",")
                            if (x.toString.contains(".")) return x.toString.indexOf(".")
                            return -1
                          }
                          ListaBuffer += x.toString.substring(initDel,endDel)
                          println("Size of ListBuffer : "+ ListaBuffer.size)



                          Mappa.update(key, ListaBuffer.toList)
                          println("La lista della mappa ha le seguenti dimensioni --> " + Mappa(key).size)

                          //controlProductions(key, Mappa(key))     sbagliato che stia qui perchè prima la mappa deve avere tutte le produzioni

    case _ => println("WARNING: " + x + " non è una produzione")


  }



  def controlTest() {

    var DXpart = Mappa.keySet.toSet.filter(_!= 'S')   // escludo nella ricerca il non terminale S perchè è l'assioma
    for(entry <- Mappa){
      println("Control Test " + entry._1 + " ---> " + entry._2)
      var key = entry._1
      var list = entry._2
      //var dxParte =
      println("DENTRO FOR Esterno analizzo la chiave >> " + key )
      if(key == 'S')  println("IL non terminale " + key + " è l'assioma")    //L'assioma deve essere unico
      else{   //produzioni del tipo A->B.... , MA B NON COMPARE IN ALTRE PRODUZIONI

        if(searchSetInList(DXpart, list) || checkIfnonTerm(list)){
          println("OK, il non terminale >> " + key + " ha ALTRE produzioni")
        }
        else{
          println("ERRORE: il non terminale >> " +  key + " non ha ALTRE produzioni")
        }

      }

      if(!entry._2.isEmpty) {
        //println("Control Test " + entry._1 + " ---> " + entry._2)
        controlProductions(entry._1, entry._2)
      }

    }

  }




  def checkIfnonTerm(list: List[String]): Boolean = {
    var atoz = ('a' to 'z').toSet
    var operator = Set('&', '/', '*', '+', '-')
    atoz = atoz ++ operator
    for(prod <- list){
        var partDx = prod.substring(2)  // parte destra
        for(car <- atoz){
          if(partDx.contains(car)){
            println("car trovato >> " + car)
            return true
          }
        }
    }
    return false
  }





  def controlProductions(nt: Char, list: List[String]): Boolean= {



      for(prod <- list){

        var auxList: List[String] = removeFromAList(prod, list)
        println("LIST---> " + list + " AUX LIST : " + auxList + " Prod -> " + prod + " nt --> " +nt)
        var dxPart = prod.substring(2)  // parte dx dopo i :
        println("DX PART --> " + dxPart)

        // produzioni del tipo A->A
        if(nt != 'S' && dxPart.contains(nt) && dxPart.size ==1){
          println("PRODUZIONE NON SIGNIFICATIVA: " + nt + "--> " +prod)
          return false
        }

        //produzioni del tipo A->....A.... , MA A NON COMPARE IN ALTRE PRODUZIONI
        if(nt != 'S' && dxPart.contains(nt) && !searchInList(nt,auxList)){      // prod count nt: mi permette di controllare il numero di occorrenze di nt nella stringa
          println("ERRORE: "+ nt+ " NON PUò ESSERE SOSTITUITO " + nt + "--> " +prod)
          return false
        }



        //produzioni del C-> S NON sono accettate
        if(dxPart.contains('S')){
          println("ERRORE: l'assioma non può comparire nella parte destra")
          return false
        }



      }

    return true
  }


  def searchInList(char:Char,list: List[String]) : Boolean =
    {
      println("Dentro searchInList...")
      for(x <- list){
        println("Cerco il carattere : " +char + " nella lista : " + list)
        if(x.contains(char)) {
          println("CARATTERE TROVATO")
          return true
        }
      }
      println("CARATTERE NON TROVATO")
      return false

    }


  def searchSetInList(set: Set[Char], list: List[String]) :Boolean =
  {
    for(car <- set){
      println("Dentro search Set In List...")
      for(x <- list){
        println("Cerco il carattere : " +car + " nella lista : " + list)
        if(x.contains(car)) {
          println("CARATTERE TROVATO")
          return true
        }
      }
      println("CARATTERE NON TROVATO")
      return false
    }

    return false
  }






  def removeFromAList(del: String, lista: List[String]): List[String] = {
    for(x <- lista){
      if(x.compareTo(del)==0) {
        //println("STAMPO LISTA : " + lista + " dopo aver eliminato da questa >> " + del)
        return lista.filter(_ != del)
      }
    }
    return lista
  }




  def getContentMap()= {
    println("Contenuto Mappa:")
    Mappa foreach(x => println("Non Terminale: " + x._1 + "---> " + x._2))

  }




}

