package com.softwaremill.quicklens

import scala.quoted.*

object QuicklensMacros {
  def toPathModify[S: Type, A: Type](obj: Expr[S], f: Expr[(A => A) => S])(using Quotes): Expr[PathModify[S, A]] = '{ PathModify( ${obj}, ${f} ) }

  def fromPathModify[S: Type, A: Type](pathModify: Expr[PathModify[S, A]])(using Quotes): Expr[(A => A) => S] = '{ ${pathModify}.f }

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{ (x: T) => ${ f('x) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] = (x: Expr[T]) => '{ $f($x) }

  def modifyLensApplyImpl[T, U](path: Expr[T => U])(using Quotes, Type[T], Type[U]): Expr[PathLazyModify[T, U]] =
    '{PathLazyModify((t, mod) => ${modifyImpl('t, path)}.using(mod))}

  def modifyAllLensApplyImpl[T, U](path1: Expr[T => U], paths: Expr[Seq[T => U]])(using Quotes, Type[T], Type[U]): Expr[PathLazyModify[T, U]] =
    '{PathLazyModify((t, mod) => ${modifyAllImpl('t, path1, paths)}.using(mod))}

  def modifyAllImpl[S, A](obj: Expr[S], focus: Expr[S => A], focusesExpr: Expr[Seq[S => A]])(using Quotes, Type[S], Type[A]): Expr[PathModify[S, A]] = {
    import quotes.reflect.*

    val focuses = focusesExpr match {
      case Varargs(args) => args
    }

    val modF1 = fromPathModify(modifyImpl(obj, focus))
    val modF = to[(A => A), S] { (mod: Expr[A => A]) =>
      focuses.foldLeft(from[(A => A), S](modF1).apply(mod)) {
        case (objAcc, focus) =>
          val modCur = fromPathModify(modifyImpl(objAcc, focus))
          from[(A => A), S](modCur).apply(mod)
      }
    }

    toPathModify(obj, modF)
  }

  def modifyImpl[S, A](obj: Expr[S], focus: Expr[S => A])(using Quotes, Type[S], Type[A]): Expr[PathModify[S, A]] = {
    import quotes.reflect.*

    def unsupportedShapeInfo(tree: Tree) = s"Unsupported path element. Path must have shape: _.field1.field2.each.field3.(...), got: ${tree.show}"

    def methodSupported(method: String) =
      Seq("at", "each", "eachWhere", "eachRight", "eachLeft", "atOrElse", "index", "when").contains(method)

    enum PathSymbol:
      case Field(name: String)
      case FunctionDelegate(name: String, givn: Term, typeTree: TypeTree, args: List[Term])

    def toPath(tree: Tree): Seq[PathSymbol] = {
      tree match {
        /** Field access */
        case Select(deep, ident) =>
          toPath(deep) :+ PathSymbol.Field(ident)
        /** Method call with arguments and using clause */
        case Apply(Apply(Apply(TypeApply(Ident(s), typeTrees), idents), args), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, args)
        /** Method call with no arguments and using clause */
        case Apply(Apply(TypeApply(Ident(s), typeTrees), idents), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Method call with one type parameter and using clause */
        case a@Apply(TypeApply(Apply(TypeApply(Ident(s), _), idents), typeTrees), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Field access */
        case Apply(deep, idents) =>
          toPath(deep) ++ idents.flatMap(toPath)
        /** Wild card from path */
        case i: Ident if i.name.startsWith("_") =>
          Seq.empty
        case _ =>
          report.throwError(unsupportedShapeInfo(focus.asTerm))
      }
    }

    def termMethodByNameUnsafe(term: Term, name: String): Symbol = {
      term.tpe.typeSymbol.memberMethod(name).head
    }

    def termAccessorMethodByNameUnsafe(term: Term, name: String): (Symbol, Int) = {
      val caseFields = term.tpe.typeSymbol.caseFields
      val idx = caseFields.map(_.name).indexOf(name)
      (caseFields.find(_.name == name).get, idx+1)
    }

    def caseClassCopy(owner: Symbol, mod: Expr[A => A], obj: Term, field: PathSymbol.Field, tail: Seq[PathSymbol]): Term =
      val objSymbol = obj.tpe.typeSymbol
      if objSymbol.flags.is(Flags.Case) then
        val copy = termMethodByNameUnsafe(obj, "copy")
        val (fieldMethod, idx) = termAccessorMethodByNameUnsafe(obj, field.name)
        val namedArg = NamedArg(field.name, mapToCopy(owner, mod, Select(obj, fieldMethod), tail))
        val fieldsIdxs = 1.to(obj.tpe.typeSymbol.caseFields.length)
        val args = fieldsIdxs.map { i =>
          if i == idx then namedArg
          else Select(obj, termMethodByNameUnsafe(obj, "copy$default$" + i.toString))
        }.toList

        obj.tpe.widen match {
          // if the object's type is parametrised, we need to call .copy with the same type parameters
          case AppliedType(_, typeParams) => Apply(TypeApply(Select(obj, copy), typeParams.map(Inferred(_))), args)
          case _ => Apply(Select(obj, copy), args)
        }
      else if (objSymbol.flags.is(Flags.Sealed) && objSymbol.flags.is(Flags.Trait)) || objSymbol.flags.is(Flags.Enum) then
        // if the source is a sealed trait / enum, generating a if-then-else with a .copy for each child (implementing case class)
        val cases = obj.tpe.typeSymbol.children.map { child =>
          val subtype = TypeIdent(child)
          val bind = Symbol.newBind(owner, "c", Flags.EmptyFlags, subtype.tpe)
          CaseDef(Bind(bind, Typed(Ref(bind), subtype)), None, caseClassCopy(owner, mod, Ref(bind), field, tail))
        }

        /*
        if (obj.isInstanceOf[Child1]) caseClassCopy(obj.asInstanceOf[Child1]) else
        if (obj.isInstanceOf[Child2]) caseClassCopy(obj.asInstanceOf[Child2]) else
        ...
        else throw new IllegalStateException()
         */
        val ifThens = obj.tpe.typeSymbol.children.map { child =>
          val ifCond = TypeApply(Select.unique(obj, "isInstanceOf"), List(TypeIdent(child)))

          val ifThen = ValDef.let(owner, TypeApply(Select.unique(obj, "asInstanceOf"), List(TypeIdent(child)))) { castToChildVal =>
            caseClassCopy(owner, mod, castToChildVal, field, tail)
          }

          ifCond -> ifThen
        }

        val elseThrow = '{throw new IllegalStateException()}.asTerm
        ifThens.foldRight(elseThrow) { case ((ifCond, ifThen), ifElse) =>
          If(ifCond, ifThen, ifElse)
        }
      else
        report.throwError(s"Unsupported source object: must be a case class or sealed trait, but got: $objSymbol")

    def mapToCopy(owner: Symbol, mod: Expr[A => A], objTerm: Term, path: Seq[PathSymbol]): Term = path match
      case Nil =>
        val apply = termMethodByNameUnsafe(mod.asTerm, "apply")
        Apply(Select(mod.asTerm, apply), List(objTerm))
      case (field: PathSymbol.Field) :: tail =>
        caseClassCopy(owner, mod, objTerm, field, tail)

      /**
        * For FunctionDelegate(method, givn, T, args)
        *
        * Generates:
        *   `givn.method[T](obj, x => mapToCopy(...), ...args)`
        *
        */
      case (f: PathSymbol.FunctionDelegate) :: tail =>
        val defdefSymbol = Symbol.newMethod(
          owner,
          "$anonfun",
          MethodType(List("x"))(_ => List(f.typeTree.tpe), _ => f.typeTree.tpe)
        )
        val fMethod = termMethodByNameUnsafe(f.givn, f.name)
        val fun = TypeApply(
          Select(f.givn, fMethod),
          List(f.typeTree)
        )
        val defdefStatements = DefDef(
          defdefSymbol, {
            case List(List(x)) => Some(mapToCopy(defdefSymbol, mod, x.asExpr.asTerm, tail))
          }
        )
        val closure = Closure(Ref(defdefSymbol), None)
        val block = Block(List(defdefStatements), closure)
        Apply(fun, List(objTerm, block) ++ f.args)

    val focusTree: Tree = focus.asTerm
    val path = focusTree match {
      /** Single inlined path */
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(p))), _)) =>
        toPath(p)
      /** One of paths from modifyAll */
      case Block(List(DefDef(_, _, _, Some(p))), _) =>
        toPath(p)
      case _ =>
        report.throwError(unsupportedShapeInfo(focusTree))
    }

    val res: (Expr[A => A] => Expr[S]) = (mod: Expr[A => A]) => mapToCopy(Symbol.spliceOwner, mod, obj.asTerm, path).asExpr.asInstanceOf[Expr[S]]
    toPathModify(obj, to(res))
  }
}
