package cryptohack.general

import play.api.libs.json.{JsObject, JsString, Json}

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket
import java.util.Base64

object Encoding {
  def bytesToString(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString

  def stringToBytes(string: String): Array[Byte] = string.getBytes

  def stringToBigInt(string: String, radix: Int = 10): BigInt = BigInt(string, radix)

  def bigIntToBytes(bigInt: BigInt): Array[Byte] = bigInt.toByteArray

  def base64Encode(bytes: Array[Byte]): Array[Byte] = Base64.getEncoder.encode(bytes)
  def base64Decode(bytes: Array[Byte]): Array[Byte] = Base64.getDecoder.decode(bytes)

  def rot13(s: String): String = s.map(x =>
    x match {
      case c if 'A' to 'Z' contains c => ('A' to 'Z')(((c.toInt - 'A') + 13) % 26)
      case z if 'a' to 'z' contains z => ('a' to 'z')(((z.toInt - 'a') + 13) % 26)
      case other                      => other
    }
  )

  def hexToBytes(string: String): Array[Byte] = bigIntToBytes(stringToBigInt(string, 16))

  def bytesToHex(bytes: Array[Byte]): Array[String] = bytes.map(e => BigInt(1, Array(e)).toString(16))


  def bytesToBigInt(bytes: Array[Byte]): BigInt = BigInt(1, bytes)



  def socketConnection(): Unit = {
    val socket = new Socket("socket.cryptohack.org", 13377)
    val in     = new BufferedReader(new InputStreamReader(socket.getInputStream))
    val out    = new PrintWriter(socket.getOutputStream, true)
    (1 to 101).foreach { _ =>
      val rawMessage = in.readLine()
      val message    = Json.parse(rawMessage)
      val encodedMessage = message \ "encoded"
      val string = (message \ "type").as[String] match {
        case "base64" => bytesToString(base64Decode(stringToBytes(encodedMessage.as[String])))
        case "hex"    => bytesToString(bigIntToBytes(stringToBigInt(encodedMessage.as[String], 16)))
        case "rot13"  => rot13(encodedMessage.as[String])
        case "bigint" => bytesToString(bigIntToBytes(stringToBigInt(encodedMessage.as[String].drop(2), 16)))
        case "utf-8"  => bytesToString(encodedMessage.as[Array[Byte]])
      }
      val result = Json.stringify(JsObject(Map("decoded" -> JsString(string))))

      out.println(result)
    }
  }
}
