package reasoner.incremental.jtms

/**
  * some cases cannot be handled by reasoner.incremental.jtms. throwing this exception indicates such a case.
  *
  * Created by hb on 5/30/16.
  */
class IncrementalUpdateFailureException(msg:String = "") extends Exception
