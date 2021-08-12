package cryptohack.general

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import cryptohack.general.Encoding.{bytesToString, hexToBytes}

object XOR {

  def xor2: String = {
    val k1             = hexToBytes("a6c8b6733c9b22de7bc0253266a3867df55acde8635e19c73313").reverse
    val k1xor2         = hexToBytes("37dcb292030faa90d07eec17e3b1c6d8daf94c35d4c9191a5e1e").reverse
    val k2xor3         = hexToBytes("c1545756687e7573db23aa1c3452a098b71a7fbf0fddddde5fc1").reverse
    val kfxor1xor2xor3 = hexToBytes("04ee9855208a2cd59091d04767ae47963170d1660df7f56f5faf").reverse

    val k2 = k1xor2.zipAll(k1, 0.toByte, 0.toByte).map(e => e._1 ^ e._2).map(_.toByte)
    val k3 = k2xor3.zipAll(k2, 0.toByte, 0.toByte).map(e => e._1 ^ e._2).map(_.toByte)

    val k5   = k1.zipAll(k2, 0.toByte, 0.toByte).map(e => e._1 ^ e._2).zip(k3).map(e => e._1 ^ e._2)
    val flag = kfxor1xor2xor3.zip(k5).map(e => e._1 ^ e._2)

    flag.map(_.toChar).reverse.mkString
  }

  def xor3: String =
    (0 to 256).map(i => bytesToString(hexToBytes("73626960647f6b206821204f21254f7d694f7624662065622127234f726927756d").map(_ ^ i.toByte).map(_.toByte))).find(_.startsWith("crypto")).getOrElse("No")

  def xor4: String = {
    val input      = "0e0b213f26041e480b26217f27342e175d0e070a3c5b103e2526217f27342e175d0e077e263451150104"
    val bruteforce = (0 to 256).map(i => bytesToString(hexToBytes(input).map(_ ^ i.toByte).map(_.toByte))).zipWithIndex
    val f          = bruteforce.filter(e => e._1.matches("(c|.r|.{2}y|.{3}p|.{4}t|.{5}o|.{6}\\{).+")).map(e => (e._1.take(8), e._2))
    val key = Seq(109, 121, 88, 79, 82, 107, 101, 121, 109, 121, 88, 79, 82, 107, 101, 121, 109, 121, 88, 79, 82, 107, 101, 121, 109, 121, 88, 79, 82, 107, 101, 121, 109, 121, 88, 79, 82, 107, 101,
      121, 109, 121, 88, 79, 82, 107, 101, 121)
    key.zip(hexToBytes(input)).map(e => e._1 ^ e._2).map(_.toByte).map(_.toChar).mkString
  }

  def xor5: String = {
    val flag  = ImmutableImage.loader().fromFile("/home/pbagriy/Downloads/flag_7ae18c704272532658c10b5faad06d74.png")
    val lemur = ImmutableImage.loader().fromFile("/home/pbagriy/Downloads/lemur_ed66878c338e662d3473f0d98eedbd0d.png")

    val pixels = lemur.pixels.zip(flag.pixels).map(e => new Pixel(e._1.x, e._1.y, e._1.red() ^ e._2.red(), e._1.green() ^ e._2.green(), e._1.blue() ^ e._2.blue(), 255))

    ImmutableImage.create(lemur.width, lemur.height, pixels).output(new JpegWriter(), "/home/pbagriy/temp/lemurs/test.jpg")

    "ok"
  }

}
