package jtms

/**
  * some cases cannot be handled by jtms. throwing this exception indicates such a case.
  *
  * Created by hb on 5/30/16.
  */
class IncrementalUpdateFailureException(msg:String = "") extends Exception
