package com.company.lib

import util.Random
import collection.immutable.HashMap
import annotation.tailrec
import net.liftweb.common.{Full, Box}

case class LinkAlphabet(prefixAlphabet : String, base : Int, alphabet : HashMap[String, Int], stringRanges : List[Range])

object IdConverter
{
  //The range in the ascii table to get the characters
  lazy val upperCaseLetters = 65 to 90
  lazy val lowerCaseLetters = 97 to 122
  lazy val numbers = 48 to 57
  lazy val symbols = List(upperCaseLetters, lowerCaseLetters, numbers)
  lazy val asciiRange = 48 to 122
  lazy val randomGen = new Random()
  lazy val rangeSeparator = "."

  /**
   * This method is used to create new alphabet to encode ids
   * @param excludedChars a list of not wanted chars in the alphabet
   * @param base the base of the encoding should not be greater than 59 and greater than one (the sum of upper case and lower case letters and numbers)
   * @return A set of a new alphabet
   */
  def CreateAlphabet(excludedChars : List[String])(base : Int, prefix : String, stringRanges : List[Range]) : LinkAlphabet =
  {
    require(base > 1 && base <= 62)
    // creates a set containing the symbols of the alphabet excluding those from the first parameter
    val initialSet = Set(symbols.flatMap(_.toSet).filter(i => !excludedChars.contains(int2String(i))):_*)
    if(initialSet.size < base)
      throw new Exception("More symbols are needed to create the alphabet.")
    @tailrec
    def findNextAlphabetChar(currentBase : Int, currentAlphabet : LinkAlphabet, currentSet : Set[Int]) : LinkAlphabet =
    {
      @tailrec
      def randomChar : Int =
      {
        val nro = getOneFromSet(currentSet)
        val genChar : String = nro
        if(currentAlphabet.alphabet.contains(genChar))
          randomChar
        else
          nro
      }
      if (currentBase == base)
        currentAlphabet
      else
      {
        val nro = randomChar
        findNextAlphabetChar(currentBase + 1, LinkAlphabet(currentAlphabet.prefixAlphabet, currentBase+1, currentAlphabet.alphabet + ((nro, currentBase)), currentAlphabet.stringRanges), currentSet.-(nro))
      }
    }
    def getOneFromSet[T](s : Set[T]) : T =
    {
      val index = randomGen.nextInt(s.size)
      s.toList(index)
    }
    findNextAlphabetChar(0, LinkAlphabet(prefix, base, HashMap(), stringRanges), initialSet)
  }

  /**
   * Creates a new alphabet without excluded characters
   * @param base the base of the encoding
   * @param prefix a prefix for the encoding
   * @return A new alphabet
   */
  def GetNewDefaultAlphabet(base : Int, prefix : String, stringRanges : List[Range]) : LinkAlphabet =
  {
    CreateAlphabet(Nil)(base, prefix, stringRanges)
  }

  /**
   * Gets the id of the sort link using the given alphabet
   * @param alpha The alphabet to be used in the encoding
   * @param id The object id from mongodb
   * @return The string id to be used in the link
   */
  def GetShortLink(alpha : LinkAlphabet)(id : String, idBase : Int) : String =
  {
    // We broke the string using the ranges of the alphabet
    val brokenId = alpha.stringRanges.foldLeft[List[String]](Nil)((list, range) => list.::(id.substring(range.start, range.end))).reverse
    val toProcess = if(brokenId.length == 0) List(id) else brokenId
    @tailrec
    def generateLink(currentList : List[String], currentLink : String) : String =
    {
      currentList match
      {
        case h::t => if (t.length > 0) generateLink(t,currentLink + encode(h) + rangeSeparator) else generateLink(t,currentLink + encode(h))
        case Nil => currentLink
      }
    }
    def encode(currentPiece : String) : String =
    {
      val factorized : List[Int] = baseFactorsConverter(idBase, currentPiece, alpha.base)
      @tailrec
      def innerEncode(factorization : List[Int], currentEncode : String) : String =
      {
        factorization match
        {
          case h::t => innerEncode(t,
                                    currentEncode +
                                    (alpha.alphabet.find(_._2 == h) match
                                    {
                                      case Some(x) => x._1
                                      case None => throw new Exception("Cannot be encoded using the current alphabet")
                                    }))
          case Nil => currentEncode
        }
      }
      innerEncode(factorized, "")
    }
    generateLink(toProcess, initialLink(alpha.prefixAlphabet))
  }

  private def initialLink(prefix : String) =
  {
    if (prefix == "default")
      ""
    else
      prefix
  }
  /**
   * We need to apply the inverse function to get the corresponding object id
   * @param alpha The alphabet to be used in the decoding
   * @param converter to convert the integer to the original base
   * @param sid The string obtained from the url
   * @return If the string is a valid object id returns a full box, if not returns Empty box
   */
  def GetObjectId(alpha : LinkAlphabet)(converter : Int => String)(sid : String) : Box[String] =
  {
    val initialPrefix = initialLink(alpha.prefixAlphabet)
    require(sid.startsWith(initialPrefix))
    val strId = sid.substring(initialPrefix.length, sid.length)
    val transformArray = strId.split(rangeSeparator)
    def getNumber(number : String) : Int =
    {
      number.foldLeft[(Int,Int)]((0 -> 0))((pair, char) => (pair._1 + (alpha.alphabet.get(char).get * (math.pow(alpha.base, pair._2).toInt)), pair._2 + 1))._1
    }
    def completeZeros(symbol : String, index : Int) : String =
    {
        (1 to (if(alpha.stringRanges.length > 0) alpha.stringRanges(index).length - symbol.length - 1 else 0)).foldLeft("")((p, zero) => p + "0") + symbol
    }
    val transformed = transformArray.map(getNumber).map(converter)
    Full(transformed.foldLeft((""->0))((p, symbol) => (p._1 + completeZeros(symbol, p._2), p._2+1))._1)
  }

  implicit def int2String(number : Int) : String =
  {
    number.toChar.toString
  }

  implicit def char2String(char : Char) : String =
  {
    char.toString
  }
  /**
   * This is used to transform from one base to another
   * @param inputBase is used to transform
   * @param inputNumber the input number to transform
   * @param outputBase the base to be used to transform
   * @return the number in the outputbase
   */
  def baseFactorsConverter(inputBase : Int, inputNumber : String, outputBase : Int) : List[Int] =
  {
    @tailrec
    def convertion(currentNm : Int, digits : List[Int]): List[Int] =
    {
      if(currentNm > 0)
      {
        convertion(currentNm / outputBase, digits.::(currentNm % outputBase))
      }
      else
        digits
    }
    convertion(Integer.parseInt(inputNumber, inputBase), Nil).reverse
  }
}
