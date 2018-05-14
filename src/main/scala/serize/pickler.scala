package serize

import boopickle._
import serize.macros.{WithCaseClassMacro, WithCaseObjectMacro}

import scala.annotation.implicitNotFound

trait Default extends Base
  with BasicImplicitPicklers
  with TransformPicklers
  with TuplePicklers
  with SerizePicklers

trait SerizePicklers extends LowPrioritySerizePicklers {
  val container: Container
}

trait LowPrioritySerizePicklers { self: SerizePicklers =>
  implicit def serizeMessagePickler[A]: Pickler[A] = new ContainerAdapter[A](container)
}

/** Turn a container into a pickler for a generic type */
class ContainerAdapter[A](container: Container) extends Pickler[A] {
  def pickle(obj: A)(implicit state: PickleState): Unit = container.pickle[A](obj)
  def unpickle(implicit state: UnpickleState): A = container.unpickle[A]
}

/** Empty container */
object Container extends Container
/** Stores how to serialize certain case classes */
class Container private(
  private val classToId: Map[Class[_], CasePickler[_]],
  private val idToClass: Map[String, CasePickler[_]],
) {
  def this() = this(Map(), Map())

  private def withPickler[A](pickler: CasePickler[A]) = {
    if(classToId contains pickler.typ)throw new PicklerAlreadyRegisteredException(s"Class ${pickler.typ.getName} is already registered")
    if(idToClass contains pickler.id)throw new PicklerAlreadyRegisteredException(s"A class with id ${pickler.id} is already registered")

    new Container(classToId + (pickler.typ -> pickler), idToClass + (pickler.id -> pickler))
  }

  def withCaseObject[M <: SerizeMessage](m: M)(implicit pickler: CasePickler[M]) = withPickler(pickler)
  def withCaseClass[C <: SerizeMessage](implicit pickler: CasePickler[C]) = withPickler(pickler)

  import scala.language.experimental.macros
  def withCaseObject[M](m: M, name: String): Container = macro WithCaseObjectMacro.impl[M]
  def withCaseClass[C](name: String)(fields: F*): Container = macro WithCaseClassMacro.impl[C]

  def pickle[A](obj: A)(implicit state: PickleState): Unit = {
    classToId get obj.getClass match {
      case Some(pickler) =>
        state.enc.writeString(pickler.id)
        pickler.asInstanceOf[CasePickler[A]].pickleFields(obj)

      case None => throw new UnregisteredPicklerException(s"Class ${obj.getClass.getName} is not registered")
    }
  }

  def unpickle[A](implicit state: UnpickleState): A = {
    val id = state.dec.readString
    idToClass get id match {
      case Some(pickler) => pickler.asInstanceOf[CasePickler[A]].unpickleData
      case None => throw new UnregisteredPicklerException(s"No case class is registered for id ${id}")
    }
  }
}

/** The @serize annotation will provide an implicit object that implements this class.
  * It pickles and unpickles the fields of a case class.
  */
@implicitNotFound("Case class ${A} must have a @serize annotation")
trait CasePickler[A] {
  /** Annotated id of the case class/object */
  val id: String

  /** class of type that will be (un)pickled */
  def typ: Class[A]

  /** pickle all fields */
  def pickleFields(obj: A)(implicit state: PickleState): Unit

  /** unpickle all fields and create the case class/object */
  def unpickleData(implicit state: UnpickleState): A
}
