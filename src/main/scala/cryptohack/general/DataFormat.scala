package cryptohack.general

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object DataFormat {

  def parse(array: Array[Byte], currentIndex: Int = 0, level: Int = 0): String = {

    if (currentIndex >= array.length) "Done"
    else if (array.drop(currentIndex).head == 0) "Unexpected exit"
    else {
      val dataType = array.drop(currentIndex).head
      val (valueSize, headerSize) =
        if ((array.drop(currentIndex + 1).head >> 7 & 1) == 0) (array.drop(currentIndex + 1).head.toInt, 1)
        else {
          val headerSize = array.drop(currentIndex + 1).head & 0x0f

          (BigInt(1, array.slice(currentIndex + 2, currentIndex + headerSize + 2)).toInt, headerSize + 1)
        }

      val value = array.slice(currentIndex + headerSize + 1, currentIndex + headerSize + valueSize + 1)


      dataType match {
        case 2                 => println(" " * (level * 2) + s" Integer $valueSize ${Encoding.bytesToBigInt(value)}")
        case 48 | 49           =>
          println(" " * (level * 2) + s" ${if (dataType == 48) "Sequence" else "Set"} $valueSize")
          parse(value, 0, level + 1)
        case 19 | 22 | 23 | 12 => println(" " * (level * 2) + s" String $valueSize ${Encoding.bytesToString(value)}")
        case 5 => println(" " * (level * 2) + s" Null")
        case 3 =>
          println(" " * (level * 2) + s" Bit String $valueSize ${value.head}")
          if (value.tail.head < 50) parse(value.tail, 0, level + 1)
        case _                 => println(" " * (level * 2) + s" $dataType $valueSize ${value.mkString(", ")}")
      }

      parse(array, currentIndex + valueSize + headerSize + 1, level)
    }
  }

  def privacyEnhancedMail(): Unit = {
    val lines = Files.readAllLines(Paths.get("/home/pbagriy/Downloads/privacy_enhanced_mail_1f696c053d76a78c2c531bb013a92d4a.pem"))

    val base64 = lines.asScala.drop(1).dropRight(1).mkString

    val decodedBytes = Encoding.base64Decode(Encoding.stringToBytes(base64))

    parse(decodedBytes.drop(4))
  }

  def certainlyNot(): Unit = {
    val bytes = Files.readAllBytes(Paths.get("/home/pbagriy/Downloads/2048b-rsa-example-cert_3220bd92e30015fe4fbeb84a755e7ca5.der"))

    parse(bytes.drop(4))
  }

}
