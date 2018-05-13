import java.nio.{ByteBuffer, ByteOrder}

package object serize {
  implicit class ByteArrayOps(bytes: Array[Byte]) {
    def toByteBuffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
  }

  implicit class ByteBufferOps(buffer: ByteBuffer) {
    def toByteArray = {
      val array = new Array[Byte](buffer.remaining)
      buffer.get(array)
      array
    }
  }
}
