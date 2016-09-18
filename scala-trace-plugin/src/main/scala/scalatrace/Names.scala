package scalatrace

abstract class Name(val name: String) {
  val up: Boolean
}

class UpName(override val name: String) extends Name(name) {
  override val up = true
}

class DownName(override val name: String) extends Name(name) {
  override val up = false
}