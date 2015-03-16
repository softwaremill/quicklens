package com.softwaremill.quicklens

object TestData {
  def duplicate(s: String) = s+s

  case class A1(a2: A2)
  case class A2(a3: A3)
  case class A3(a4: A4)
  case class A4(a5: A5)
  case class A5(name: String)

  val a5 = A5("data")
  val a5dup = A5("datadata")

  val a1 = A1(A2(A3(A4(A5("data")))))
  val a1dup = A1(A2(A3(A4(A5("datadata")))))
  val a1mod = A1(A2(A3(A4(A5("mod")))))

  case class X1(x2: X2)
  case class X2(x3: Option[X3])
  case class X3(x4: X4)
  case class X4(x5: Option[X5])
  case class X5(name: String)

  val x4 = X4(Some(X5("data")))
  val x4dup = X4(Some(X5("datadata")))

  val x4none = X4(None)

  val x1 = X1(X2(Some(X3(X4(Some(X5("data")))))))
  val x1dup = X1(X2(Some(X3(X4(Some(X5("datadata")))))))

  val x1none = X1(X2(None))

  case class Y1(y2: Y2)
  case class Y2(y3: List[Y3])
  case class Y3(y4: Option[Y4])
  case class Y4(name: String)

  val y1 = Y1(Y2(List(Y3(Some(Y4("d1"))), Y3(Some(Y4("d2"))), Y3(None))))
  val y1dup = Y1(Y2(List(Y3(Some(Y4("d1d1"))), Y3(Some(Y4("d2d2"))), Y3(None))))

  case class Z1(name: Option[String])
  val z1 = Z1(Some("data"))
  val z1dup = Z1(Some("datadata"))
}
