package fr.emalios.algointerpreter.typecheck

object ReturnMarker extends Enumeration {
  type ReturnMarker = Value

  val RETURN, MAY_RETURN, NO_RETURN = Value

  def combineAnd(returnMarker: ReturnMarker, returnMarker2: ReturnMarker): ReturnMarker = {
    (returnMarker, returnMarker2) match {
      case (RETURN, RETURN) => RETURN
      case (RETURN, _) => MAY_RETURN
      case (_, RETURN) => MAY_RETURN
      case (MAY_RETURN, _) => MAY_RETURN
      case (_, MAY_RETURN) => MAY_RETURN
      case _ => NO_RETURN
    }
  }

  def combineOr(returnMarker: ReturnMarker, returnMarker2: ReturnMarker): ReturnMarker = {
    (returnMarker, returnMarker2) match {
      case (RETURN, _) => RETURN
      case (_, RETURN) => RETURN
      case (MAY_RETURN, _) => MAY_RETURN
      case (_, MAY_RETURN) => MAY_RETURN
      case _ => NO_RETURN
    }
  }

  def relax(returnMarker: ReturnMarker): ReturnMarker = {
    returnMarker match {
      case RETURN => MAY_RETURN
      case actual@_ => actual
    }
  }

}
