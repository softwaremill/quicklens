package com.softwaremill.quicklens

import scala.quoted.*

object QuicklensMacros {
  def toPathModify[S: Type, A: Type](obj: Expr[S], f: Expr[(A => A) => S])(using Quotes): Expr[PathModify[S, A]] = '{
    PathModify(${ obj }, ${ f })
  }

  def fromPathModify[S: Type, A: Type](pathModify: Expr[PathModify[S, A]])(using Quotes): Expr[(A => A) => S] = '{
    ${ pathModify }.f
  }

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{ (x: T) => ${ f('x) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] = (x: Expr[T]) => '{ $f($x) }

  def modifyLensApplyImpl[T, U](path: Expr[T => U])(using Quotes, Type[T], Type[U]): Expr[PathLazyModify[T, U]] = '{
    PathLazyModify { (t, mod) =>
      ${
        toPathModify('t, modifyImpl('t, Seq(path)))
      }.using(mod)
    }
  }

  def modifyAllLensApplyImpl[T: Type, U: Type](
      path1: Expr[T => U],
      paths: Expr[Seq[T => U]]
  )(using Quotes): Expr[PathLazyModify[T, U]] =
    '{ PathLazyModify((t, mod) => ${ modifyAllImpl('t, path1, paths) }.using(mod)) }

  def modifyAllImpl[S: Type, A: Type](
      obj: Expr[S],
      focus: Expr[S => A],
      focusesExpr: Expr[Seq[S => A]]
  )(using Quotes): Expr[PathModify[S, A]] = {
    import quotes.reflect.*

    val focuses = focusesExpr match {
      case Varargs(args) => focus +: args
    }

    val modF = modifyImpl(obj, focuses)

    toPathModify(obj, modF)
  }

  def toPathModifyFromFocus[S: Type, A: Type](obj: Expr[S], focus: Expr[S => A])(using Quotes): Expr[PathModify[S, A]] =
    toPathModify(obj, modifyImpl(obj, Seq(focus)))

  private def modifyImpl[S: Type, A: Type](
      obj: Expr[S],
      focuses: Seq[Expr[S => A]]
  )(using Quotes): Expr[(A => A) => S] = {
    import quotes.reflect.*

    def unsupportedShapeInfo(tree: Tree) =
      s"Unsupported path element. Path must have shape: _.field1.field2.each.field3.(...), got: ${tree.show}"

    def noSuchMember(tpeStr: String, name: String) =
      s"$tpeStr has no member named $name"

    def methodSupported(method: String) =
      Seq("at", "each", "eachWhere", "eachRight", "eachLeft", "atOrElse", "index", "when").contains(method)

    enum PathTree:
      case Empty
      case Node(children: Seq[(PathSymbol, Seq[PathTree])])

      def <>(symbols: Seq[PathSymbol]): PathTree = ((this, symbols): @unchecked) match
        case (PathTree.Empty, _) =>
          symbols.toPathTree
        case (PathTree.Node(children), (symbol :: Nil)) =>
          PathTree.Node {
            if children.find(_._1 equiv symbol).isEmpty then children :+ (symbol -> Seq(PathTree.Empty))
            else
              children.map {
                case (sym, trees) if sym equiv symbol =>
                  sym -> (trees :+ PathTree.Empty)
                case c => c
              }
          }
        case (PathTree.Node(children), Nil) =>
          this
        case (PathTree.Node(children), (symbol :: tail)) =>
          PathTree.Node {
            if children.find(_._1 equiv symbol).isEmpty then children :+ (symbol -> Seq(tail.toPathTree))
            else
              children.map {
                case (sym, trees) if sym equiv symbol =>
                  sym -> (trees.init ++ {
                    trees.last match
                      case PathTree.Empty => Seq(PathTree.Empty, tail.toPathTree)
                      case node           => Seq(node <> tail)
                  })
                case c => c
              }
          }
    end PathTree

    object PathTree:
      def empty: PathTree = Empty

    extension (symbols: Seq[PathSymbol])
      def toPathTree: PathTree = symbols match
        case Nil              => PathTree.Empty
        case (symbol :: tail) => PathTree.Node(Seq(symbol -> Seq(tail.toPathTree)))

    enum PathSymbol:
      case Field(name: String)
      case FunctionDelegate(name: String, givn: Term, typeTree: TypeTree, args: List[Term])

      def equiv(other: Any): Boolean = (this, other) match
        case (Field(name1), Field(name2)) => name1 == name2
        case (FunctionDelegate(name1, _, typeTree1, args1), FunctionDelegate(name2, _, typeTree2, args2)) =>
          name1 == name2 && typeTree1.tpe == typeTree2.tpe && args1 == args2
        case _ => false
    end PathSymbol

    def toPath(tree: Tree, focus: Expr[S => A]): Seq[PathSymbol] = {
      tree match {
        /** Field access */
        case Select(deep, ident) =>
          toPath(deep, focus) :+ PathSymbol.Field(ident)
        /** Method call with arguments and using clause */
        case Apply(Apply(Apply(TypeApply(Ident(s), typeTrees), idents), args), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, args)
        /** Method call with no arguments and using clause */
        case Apply(Apply(TypeApply(Ident(s), typeTrees), idents), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Method call with one type parameter and using clause */
        case a @ Apply(TypeApply(Apply(TypeApply(Ident(s), _), idents), typeTrees), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Field access */
        case Apply(deep, idents) =>
          toPath(deep, focus) ++ idents.flatMap(toPath(_, focus))
        /** Wild card from path */
        case i: Ident if i.name.startsWith("_") =>
          Seq.empty
        case _ =>
          report.errorAndAbort(unsupportedShapeInfo(focus.asTerm))
      }
    }

    extension (tpe: TypeRepr)
      def poorMansLUB: TypeRepr = tpe match {
        case AndType(l, r) if l <:< r => l
        case AndType(l, r) if r <:< l => r
        case _                        => tpe
      }
      def widenAll: TypeRepr =
        tpe.widen.dealias.poorMansLUB
      def matchingTypeSymbol: Symbol = tpe.widenAll match {
        case AndType(l, r) =>
          val lSym = l.matchingTypeSymbol
          if l.matchingTypeSymbol != Symbol.noSymbol then lSym else r.matchingTypeSymbol
        case tpe if isProduct(tpe.typeSymbol) || isSum(tpe.typeSymbol) =>
          tpe.typeSymbol
        case tpe =>
          Symbol.noSymbol
      }

    def symbolMethodByNameUnsafe(sym: Symbol, name: String): Symbol = {
      sym
        .methodMember(name)
        .headOption
        .getOrElse(report.errorAndAbort(noSuchMember(sym.name, name)))
    }

    def termMethodByNameUnsafe(term: Term, name: String): Symbol = {
      symbolMethodByNameUnsafe(term.tpe.widenAll.typeSymbol, name)
    }

    def termAccessorMethodByNameUnsafe(term: Term, name: String): (Symbol, Int) = {
      val typeSymbol = term.tpe.widenAll.matchingTypeSymbol
      val caseParamNames = typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTerm).map(_.name)
      val idx = caseParamNames.indexOf(name)
      typeSymbol.caseFields.find(_.name == name).getOrElse(report.errorAndAbort(noSuchMember(term.tpe.show, name)))
        -> (idx + 1)
    }

    def isProduct(sym: Symbol): Boolean = {
      sym.flags.is(Flags.Case)
    }

    def isSum(sym: Symbol): Boolean = {
      sym.flags.is(Flags.Enum) ||
      (sym.flags.is(Flags.Sealed) && (sym.flags.is(Flags.Trait) || sym.flags.is(Flags.Abstract)))
    }

    def caseClassCopy(
        owner: Symbol,
        mod: Expr[A => A],
        obj: Term,
        fields: Seq[(PathSymbol.Field, Seq[PathTree])]
    ): Term = {
      val objSymbol = obj.tpe.widenAll.matchingTypeSymbol
      if isProduct(objSymbol) then {
        val copy = symbolMethodByNameUnsafe(objSymbol, "copy")
        val argsMap: Map[Int, Term] = fields.map { (field, trees) =>
          val (fieldMethod, idx) = termAccessorMethodByNameUnsafe(obj, field.name)
          val resTerm: Term = trees.foldLeft[Term](Select(obj, fieldMethod)) { (term, tree) =>
            mapToCopy(owner, mod, term, tree)
          }
          val namedArg = NamedArg(field.name, resTerm)
          idx -> namedArg
        }.toMap

        val typeParams = obj.tpe.widenAll match {
          case AppliedType(_, typeParams) => Some(typeParams)
          case _                          => None
        }
        val constructorTree: DefDef = objSymbol.primaryConstructor.tree.asInstanceOf[DefDef]
        val firstParamListLength: Int = constructorTree.termParamss.headOption.map(_.params).toList.flatten.length
        val fieldsIdxs = 1.to(firstParamListLength)
        val args = fieldsIdxs.map { i =>
          val defaultMethod = obj.select(symbolMethodByNameUnsafe(objSymbol, "copy$default$" + i.toString))
          argsMap.getOrElse(
            i,
            typeParams.fold(defaultMethod)(defaultMethod.appliedToTypes)
          )
        }.toList

        if constructorTree.termParamss.drop(1).exists(_.params.exists(!_.symbol.flags.is(Flags.Implicit))) then
          report.errorAndAbort(
            s"Implementation limitation: Only the first parameter list of the modified case classes can be non-implicit."
          )

        typeParams match {
          // if the object's type is parametrised, we need to call .copy with the same type parameters
          case Some(typeParams) => Apply(TypeApply(Select(obj, copy), typeParams.map(Inferred(_))), args)
          case _                => Apply(Select(obj, copy), args)
        }
      } else if isSum(objSymbol) then {
        obj.tpe.widenAll match {
          case AndType(_, _) =>
            report.errorAndAbort(
              s"Implementation limitation: Cannot modify sealed hierarchies mixed with & types. Try providing a more specific type."
            )
          case _ =>
        }
        /*
        if (obj.isInstanceOf[Child1]) caseClassCopy(obj.asInstanceOf[Child1]) else
        if (obj.isInstanceOf[Child2]) caseClassCopy(obj.asInstanceOf[Child2]) else
        ...
        else throw new IllegalStateException()
         */
        val ifThens = objSymbol.children.map { child =>
          val ifCond = TypeApply(Select.unique(obj, "isInstanceOf"), List(TypeIdent(child)))

          val ifThen = ValDef.let(owner, TypeApply(Select.unique(obj, "asInstanceOf"), List(TypeIdent(child)))) {
            castToChildVal =>
              caseClassCopy(owner, mod, castToChildVal, fields)
          }

          ifCond -> ifThen
        }

        val elseThrow = '{ throw new IllegalStateException() }.asTerm
        ifThens.foldRight(elseThrow) { case ((ifCond, ifThen), ifElse) =>
          If(ifCond, ifThen, ifElse)
        }
      } else
        report.errorAndAbort(s"Unsupported source object: must be a case class or sealed trait, but got: $objSymbol")
    }

    def applyFunctionDelegate(
        owner: Symbol,
        mod: Expr[A => A],
        objTerm: Term,
        f: PathSymbol.FunctionDelegate,
        tree: PathTree
    ): Term =
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
        defdefSymbol,
        ((_: @unchecked) match
          case List(List(x)) => Some(mapToCopy(defdefSymbol, mod, x.asExpr.asTerm, tree))
        )
      )
      val closure = Closure(Ref(defdefSymbol), None)
      val block = Block(List(defdefStatements), closure)
      Apply(fun, List(objTerm, block) ++ f.args.map(_.changeOwner(owner)))

    def accumulateToCopy(
        owner: Symbol,
        mod: Expr[A => A],
        objTerm: Term,
        pathSymbols: Seq[(PathSymbol, Seq[PathTree])]
    ): Term = pathSymbols match {

      case Nil =>
        objTerm

      case (_: PathSymbol.Field, _) :: _ =>
        val (fs, funs) = pathSymbols.span(_._1.isInstanceOf[PathSymbol.Field])
        val fields = fs.collect { case (p: PathSymbol.Field, trees) => p -> trees }
        val withCopiedFields: Term = caseClassCopy(owner, mod, objTerm, fields)
        accumulateToCopy(owner, mod, withCopiedFields, funs)

      /** For FunctionDelegate(method, givn, T, args)
        *
        * Generates: `givn.method[T](obj, x => mapToCopy(...), ...args)`
        */
      case (f: PathSymbol.FunctionDelegate, actions: Seq[PathTree]) :: tail =>
        val term = actions.foldLeft(objTerm) { (term, tree) =>
          applyFunctionDelegate(owner, mod, term, f, tree)
        }
        accumulateToCopy(owner, mod, term, tail)
    }

    def mapToCopy(owner: Symbol, mod: Expr[A => A], objTerm: Term, pathTree: PathTree): Term = pathTree match {
      case PathTree.Empty =>
        val apply = termMethodByNameUnsafe(mod.asTerm, "apply")
        Apply(Select(mod.asTerm, apply), List(objTerm))
      case PathTree.Node(children) =>
        accumulateToCopy(owner, mod, objTerm, children)
    }

    def extractFocus(tree: Tree): Tree = tree match {
      /** Single inlined path */
      case Inlined(_, _, p) =>
        extractFocus(p)
      /** One of paths from modifyAll */
      case Block(List(DefDef(_, _, _, Some(p))), _) =>
        p
      case _ =>
        report.errorAndAbort(unsupportedShapeInfo(tree))
    }

    val focusesTrees: Seq[Tree] = focuses.map(_.asTerm)
    val paths: Seq[Seq[PathSymbol]] =
      focusesTrees.zip(focuses).map { (tree, focus) =>
        toPath(extractFocus(tree), focus)
      }

    val pathTree: PathTree =
      paths.foldLeft(PathTree.empty) { (tree, path) => tree <> path }

    val res: (Expr[A => A] => Expr[S]) = (mod: Expr[A => A]) =>
      Typed(mapToCopy(Symbol.spliceOwner, mod, obj.asTerm, pathTree), TypeTree.of[S]).asExpr.asInstanceOf[Expr[S]]

    to(res)
  }
}
