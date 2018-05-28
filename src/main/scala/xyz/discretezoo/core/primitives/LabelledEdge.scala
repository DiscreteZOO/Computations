package xyz.discretezoo.core.primitives

case class LabelledEdge(_1: Int, _2: Int, label: Int) {
  def isSemiedge: String = if (_1 == _2) "true" else "false"
  def Magma: String = s"""<${_1}, {${_2}}>"""
}
