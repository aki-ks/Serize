package serize

class NoDefaultValueException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
class PicklerAlreadyRegisteredException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
class UnregisteredPicklerException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
