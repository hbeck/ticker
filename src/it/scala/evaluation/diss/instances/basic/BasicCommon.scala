package evaluation.diss.instances.basic

object BasicCommon {

  sealed abstract class Modality
  object time_at extends Modality
  object time_diamond extends Modality
  object time_box extends Modality
  object count_at extends Modality
  object count_diamond extends Modality
  object count_box extends Modality

  def modalityFromString(wm: String) = wm match {
    case "ta" => time_at
    case "td"=> time_diamond
    case "tb" => time_box
    case "ca" => count_at
    case "cd" => count_diamond
    case "cb" => count_box
  }

}
