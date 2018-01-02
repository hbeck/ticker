package reasoner.incremental.jtms

/**
  * Created by hb on 12/22/15.
  */
sealed abstract class Status

case object in extends Status

case object out extends Status

case object unknown extends Status
