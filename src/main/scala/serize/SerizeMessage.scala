package serize

/**
  * Marker trait that all serializable case classes and case object should have.
  * It's automatically added by the @serize macro annotation.
  */
trait SerizeMessage extends Product
