package com.softwaremill.quicklens

case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {
  /**
   * Transform the value of the field using the given function.
   * @return A copy of the root object with the (deeply nested) field modified.
   */
  def using(mod: U => U): T = doModify(obj, mod)
  /**
   * Set the value of the field to a new value.
   * @return A copy of the root object with the (deeply nested) field set to the new value.
   */
  def setTo(v: U): T = doModify(obj, _ => v)
}
